/*
 *  TxnImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.confluent
package impl

import de.sciss.lucre.confluent.impl.{PathImpl => Path}
import de.sciss.lucre.event.ReactionMap
import de.sciss.lucre.{event => evt, stm}
import de.sciss.lucre.stm.{Obj, Durable, InMemory, IdentifierMap}
import de.sciss.serial
import de.sciss.serial.{DataInput, ImmutableSerializer}

import scala.collection.immutable.{Queue => IQueue, IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, Txn => ScalaTxn}

trait TxnMixin[S <: Sys[S]]
  extends Txn[S] with stm.impl.BasicTxnImpl[S] with VersionInfo.Modifiable {
  _: S#Tx =>

  // ---- abstract ----

  protected def flushCaches(meld: MeldInfo[S], newVersion: Boolean, caches: Vec[Cache[S#Tx]]): Unit

  // ---- info ----

  final private[lucre] def reactionMap: ReactionMap[S] = system.reactionMap

  final def info: VersionInfo.Modifiable = this

  final var message: String = ""
  final val timeStamp: Long = System.currentTimeMillis()

  // ---- init ----

  /*
   * Because durable maps are persisted, they may be deserialized multiple times per transaction.
   * This could potentially cause a problem: imagine two instances A1 and A2. A1 is read, a `put`
   * is performed, making A1 call `markDirty`. Next, A2 is read, again a `put` performed, and A2
   * calls `markDirty`. Next, another `put` on A1 is performed. In the final flush, because A2
   * was marked after A1, it's cached value will override A2's, even though it is older.
   *
   * To avoid that, durable maps are maintained by their id's in a transaction local map. That way,
   * only one instance per id is available in a single transaction.
   */
  // private var durableIdMaps     = IntMap.empty[DurableIdMapImpl[_, _]]
  private var meld              = MeldInfo.empty[S]
  private var dirtyMaps         = Vector.empty[Cache[S#Tx]]
  private var beforeCommitFuns  = IQueue.empty[S#Tx => Unit]

  // indicates whether we have added cache maps to dirty maps
  private var markDirtyFlag       = false
  // indicates whether we have added cache maps that require version updates (e.g. non-event writes)
  private var markNewVersionFlag  = false
  // indicates whether any before commit handling is needed
  // (either dirtyMaps got non-empty, or a user before-commit handler got registered)
  private var markBeforeCommitFlag = false

  final protected def meldInfo: MeldInfo[S] = meld

  final private def markDirty(): Unit =
    if (!markDirtyFlag) {
      markDirtyFlag = true
      addDirtyCache(fullCache)
      // addDirtyCache(partialCache)
    }

  final def addDirtyCache(map: Cache[S#Tx]): Unit = {
    dirtyMaps :+= map
    markNewVersionFlag = true
    markBeforeCommit()
  }

  /** A local cache is one which is re-created upon application restart. It should
    * probably be called transient instead of local, but we already have `stm.LocalVar`...
    *
    * If the dirty maps only contain local caches, no new version is created upon flush.
    */
  final def addDirtyLocalCache(map: Cache[S#Tx]): Unit = {
    dirtyMaps :+= map
    markBeforeCommit()
  }

  final override def beforeCommit(fun: S#Tx => Unit): Unit = {
    beforeCommitFuns = beforeCommitFuns.enqueue(fun)
    markBeforeCommit()
  }

  private def markBeforeCommit(): Unit =
    if (!markBeforeCommitFlag) {
      markBeforeCommitFlag = true
      log("....... txn dirty .......")
      ScalaTxn.beforeCommit(handleBeforeCommit)(peer)
    }

  // first execute before commit handlers, then flush
  private def handleBeforeCommit(itx: InTxn): Unit = {
    while (beforeCommitFuns.nonEmpty) {
      val (fun, q) = beforeCommitFuns.dequeue
      beforeCommitFuns = q
      fun(this)
    }
    flushCaches(meld, markNewVersionFlag, dirtyMaps)
  }

  final protected def fullCache: CacheMap.Durable[S, Int, DurablePersistentMap[S, Int]] = system.fullCache
  // final protected def partialCache  = system.partialCache

  final def newId(): S#Id = {
    val res = new ConfluentId[S](system.newIdValue()(this), Path.empty[S])
    log(s"txn newId $res")
    res
  }

  // ---- context ----

  // def newContext(): S#Context = ...

  // ---- attributes ----

  def attrMap(obj: Obj[S]): Obj.AttrMap[S] = {
    val id        = obj.id
    val mBase     = id.base | 0x80000000  // XXX TODO --- a bit cheesy to throw away one bit entirely
    val mapOpt    = fullCache.getCacheTxn[Obj.AttrMap[S]](mBase, id.path)(this, Obj.attrMapSerializer)
    mapOpt.getOrElse {
      val map = evt.Map.Modifiable[S, String, Obj](evt.Map.Key.String, this)
      fullCache.putCacheTxn(mBase, id.path, map)(this, Obj.attrMapSerializer)
      markDirty()
      map
    }
  }

//  def attrGet(obj: Obj[S], key: String): Option[Obj[S]] = ...
//  def attrPut(obj: Obj[S], key: String, value: Obj[S]): Unit = ...
//  def attrRemove(obj: Obj[S], key: String): Unit = ...
//
//  def attrIterator(obj: Obj[S]): Iterator[(String, Obj[S])] = ...

  // ----

  final def readTreeVertexLevel(term: Long): Int = {
    system.store.get(out => {
      out.writeByte(0)
      out.writeInt(term.toInt)
    })(in => {
      in./* PACKED */ readInt()  // tree index!
      in./* PACKED */ readInt()
    })(this).getOrElse(sys.error(s"Trying to access non-existent vertex ${term.toInt}"))
  }

  final def addInputVersion(path: S#Acc): Unit = {
    val sem1 = path.seminal
    val sem2 = inputAccess.seminal
    if (sem1 == sem2) return
    if (sem1 != sem2) {
      val m = meld
      // note: before we were reading the index tree; but since only the level
      // is needed, we can read the vertex instead which also stores the
      // the level.
      //               val tree1 = readIndexTree(sem1.head)
      val tree1Level = readTreeVertexLevel(sem1.head)
      val m1 = if (m.isEmpty) {
        val tree2Level = readTreeVertexLevel(sem2.head)
        m.add(tree2Level, sem2)
      } else m
      meld = m1.add(tree1Level, sem1)
    }
  }

  final def newHandle[A](value: A)(implicit serializer: serial.Serializer[S#Tx, S#Acc, A]): Source[S, A] = {
    val h = new HandleImpl[S, A](value, inputAccess.index)
    addDirtyLocalCache(h)
    h
  }

  final def newHandleM[A](value: A)(implicit serializer: serial.Serializer[S#Tx, S#Acc, A]): Source[S, A] =
    newHandle(value)

  final def getNonTxn[A](id: S#Id)(implicit ser: ImmutableSerializer[A]): A = {
    log(s"txn get $id")
    fullCache.getCacheNonTxn[A](id.base, id.path)(this, ser).getOrElse(sys.error(s"No value for $id"))
  }

  final def getTxn[A](id: S#Id)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): A = {
    log(s"txn get' $id")
    fullCache.getCacheTxn[A](id.base, id.path)(this, ser).getOrElse(sys.error(s"No value for $id"))
  }

  final def putTxn[A](id: S#Id, value: A)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): Unit = {
    fullCache.putCacheTxn[A](id.base, id.path, value)(this, ser)
    markDirty()
  }

  final def putNonTxn[A](id: S#Id, value: A)(implicit ser: ImmutableSerializer[A]): Unit = {
    fullCache.putCacheNonTxn[A](id.base, id.path, value)(this, ser)
    markDirty()
  }

//  final def putPartial[A](id: S#Id, value: A)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): Unit = {
//    partialCache.putPartial(id.base, id.path, value)(this, ser)
//    markDirty()
//  }
//
//  final def getPartial[A](id: S#Id)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): A =
//    partialCache.getPartial[A](id.base, id.path)(this, ser).getOrElse(sys.error(s"No value for $id"))

  final def removeFromCache(id: S#Id): Unit =
    fullCache.removeCacheOnly(id.base, id.path)(this)

  @inline final protected def alloc       (pid: S#Id): S#Id = new ConfluentId(system.newIdValue()(this), pid.path)
  @inline final protected def allocPartial(pid: S#Id): S#Id = new PartialId  (system.newIdValue()(this), pid.path)

  final def newVar[A](pid: S#Id, init: A)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
    val res = makeVar[A](alloc(pid))
    log(s"txn newVar $res")
    res.setInit(init)(this)
    res
  }

//  final def newLocalVar[A](init: S#Tx => A): stm.LocalVar[S#Tx, A] = new stm.impl.LocalVarImpl[S, A](init)
//
//  final def newPartialVar[A](pid: S#Id, init: A)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
//    if (Confluent.DEBUG_DISABLE_PARTIAL) return newVar(pid, init)
//
//    val res = new PartialVarTxImpl[S, A](allocPartial(pid))
//    log(s"txn newPartialVar $res")
//    res.setInit(init)(this)
//    res
//  }

  final def newBooleanVar(pid: S#Id, init: Boolean): S#Var[Boolean] = {
    val id  = alloc(pid)
    val res = new BooleanVar(id)
    log(s"txn newVar $res")
    res.setInit(init)(this)
    res
  }

  final def newIntVar(pid: S#Id, init: Int): S#Var[Int] = {
    val id  = alloc(pid)
    val res = new IntVar(id)
    log(s"txn newVar $res")
    res.setInit(init)(this)
    res
  }

  final def newLongVar(pid: S#Id, init: Long): S#Var[Long] = {
    val id  = alloc(pid)
    val res = new LongVar(id)
    log(s"txn newVar $res")
    res.setInit(init)(this)
    res
  }

  final def newVarArray[A](size: Int): Array[S#Var[A]] = new Array[S#Var[A]](size)

  final def newInMemoryIdMap[A]: IdentifierMap[S#Id, S#Tx, A] = {
    val map = InMemoryConfluentMap.newIntMap[S]
    new InMemoryIdMapImpl[S, A](map)
  }

  final protected def readSource(in: DataInput, pid: S#Id): S#Id = {
    val base = in./* PACKED */ readInt()
    new ConfluentId(base, pid.path)
  }

  final protected def readPartialSource(in: DataInput, pid: S#Id): S#Id = {
    val base = in./* PACKED */ readInt()
    new PartialId(base, pid.path)
  }

  private def makeVar[A](id: S#Id)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): S#Var[A] /* BasicVar[ S, A ] */ = {
    ser match {
      case plain: ImmutableSerializer[_] =>
        new VarImpl[S, A](id, plain.asInstanceOf[ImmutableSerializer[A]])
      case _ =>
        new VarTxImpl[S, A](id)
    }
  }

  final def readVar[A](pid: S#Id, in: DataInput)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
    val res = makeVar[A](readSource(in, pid))
    log(s"txn read $res")
    res
  }

//  final def readPartialVar[A](pid: S#Id, in: DataInput)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
//    if (Confluent.DEBUG_DISABLE_PARTIAL) return readVar(pid, in)
//
//    val res = new PartialVarTxImpl[S, A](readPartialSource(in, pid))
//    log(s"txn read $res")
//    res
//  }

  final def readBooleanVar(pid: S#Id, in: DataInput): S#Var[Boolean] = {
    val res = new BooleanVar(readSource(in, pid))
    log(s"txn read $res")
    res
  }

  final def readIntVar(pid: S#Id, in: DataInput): S#Var[Int] = {
    val res = new IntVar(readSource(in, pid))
    log(s"txn read $res")
    res
  }

  final def readLongVar(pid: S#Id, in: DataInput): S#Var[Long] = {
    val res = new LongVar(readSource(in, pid))
    log(s"txn read $res")
    res
  }

  final def readId(in: DataInput, acc: S#Acc): S#Id = {
    val base  = in./* PACKED */ readInt()
    val res   = new ConfluentId(base, Path.readAndAppend[S](in, acc)(this))
    log(s"txn readId $res")
    res
  }

  override def toString = s"confluent.Sys#Tx$inputAccess"
}

trait RegularTxnMixin[S <: Sys[S], D <: stm.DurableLike[D]] extends TxnMixin[S] {
  _: S#Tx =>

  protected def cursorCache: Cache[S#Tx]

  final protected def flushCaches(meldInfo: MeldInfo[S], newVersion: Boolean, caches: Vec[Cache[S#Tx]]): Unit =
    system.flushRegular(meldInfo, newVersion, caches :+ cursorCache)(this)

  override def toString = s"Confluent#Tx$inputAccess"
}

trait RootTxnMixin[S <: Sys[S], D <: stm.DurableLike[D]]
  extends TxnMixin[S] {
  _: S#Tx =>

  final val inputAccess: S#Acc = Path.root[S]

  final def isRetroactive = false

  final protected def flushCaches(meldInfo: MeldInfo[S], newVersion: Boolean, caches: Vec[Cache[S#Tx]]): Unit =
    system.flushRoot(meldInfo, newVersion, caches)(this)

  override def toString = "Confluent.RootTxn"
}

private[impl] sealed trait TxnImpl extends Txn[Confluent] {
  final lazy val inMemory: InMemory#Tx = system.inMemory.wrap(peer)
}

private[impl] final class RegularTxn(val system: Confluent, val durable: Durable#Tx,
                               val inputAccess: Confluent#Acc, val isRetroactive: Boolean,
                               val cursorCache: Cache[Confluent#Tx])
  extends RegularTxnMixin[Confluent, Durable] with TxnImpl {

  lazy val peer: InTxn = durable.peer
}

private[impl] final class RootTxn(val system: Confluent, val peer: InTxn)
  extends RootTxnMixin[Confluent, Durable] with TxnImpl {

  lazy val durable: Durable#Tx = {
    log("txn durable")
    system.durable.wrap(peer)
  }
}