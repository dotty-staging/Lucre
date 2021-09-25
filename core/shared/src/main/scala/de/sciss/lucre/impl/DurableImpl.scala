/*
 *  DurableImpl.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.Log.{txn => logTxn}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

import scala.annotation.elidable
import scala.concurrent.stm.{InTxn, Ref}

object DurableImpl {
  private type D[T <: DurableLike.Txn[T]] = DurableLike.Txn[T]

  def apply(factory: DataStore.Factory, mainName: String): Durable = {
    val mainStore   = factory.open(mainName)
    apply(mainStore = mainStore)
  }

  def apply(mainStore: DataStore): Durable = new System(store = mainStore)

  trait Mixin[T <: D[T], I <: Txn[I]] extends DurableLike[T] with ReactionMapImpl.Mixin[T] {
//    self =>

    def store: DataStore

    protected final val eventMap: IdentMap[T, Map[Int, scala.List[Observer[T, _]]]] =
      IdentMapImpl[T, Map[Int, scala.List[Observer[T, _]]]] { implicit tx => id => id.!.id }

    private[this] val idCntVar = step { implicit tx =>
      val _id = store.get(_.writeInt(0))(_.readInt()).getOrElse(1)
      val _idCnt = Ref(_id)
      new CachedIntVar[T](0, _idCnt)
    }

    def root[A](init: T => A)(implicit format: TFormat[T, A]): Source[T, A] =
      step { implicit tx =>
        rootBody(init)
      }

    def rootJoin[A](init: T => A)(implicit tx: TxnLike, format: TFormat[T, A]): Source[T, A] =
      rootBody(init)(wrap(tx.peer), format)

    private[this] def rootBody[A](init: T => A)
                                 (implicit tx: T, format: TFormat[T, A]): Source[T, A] = {
      val rootId = 2 // 1 == reaction map!!!
      if (exists(rootId)) {
        new VarImpl[T, A](rootId, format)
      } else {
        val id = newIdValue()
        require(id === rootId,
          s"Root can only be initialized on an empty database (expected id count is $rootId but found $id)")
        val res = new VarImpl[T, A](id, format)
        res.setInit(init(tx))
        res
      }
    }

    // ---- cursor ----

    def step[A](fun: T => A): A = stepTag(0L)(fun)

    def stepTag[A](systemTimeNanos: Long)(fun: T => A): A = Txn.atomic { implicit itx =>
      fun(wrap(itx, systemTimeNanos))
    }

//    def position(implicit tx: T): Unit = ()

    type Id = DurableLike.Id[T]

    def debugListUserRecords()(implicit tx: T): Seq[Id] = {
      val b   = Seq.newBuilder[Id]
      val cnt = idCntVar()
      var i   = 1
      while (i <= cnt) {
        if (exists(i)) b += new IdImpl[T](i)
        i += 1
      }
      b.result()
    }

    def close(): Unit = store.close()

    def numRecords(implicit tx: T): Int = store.numEntries

    def numUserRecords(implicit tx: T): Int = math.max(0, numRecords - 1)

    // this increases a durable variable, thus ensures markDirty() already
    def newIdValue()(implicit tx: T): Int = {
      val id = idCntVar() + 1
      logTxn.debug(s"new   <$id>")
      idCntVar() = id
      id
    }

    def write(id: Long)(valueFun: DataOutput => Unit)(implicit tx: T): Unit = {
      logTxn.debug(s"writeL <$id>")
      store.put(_.writeLong(id))(valueFun)
    }

    def write(id: Int)(valueFun: DataOutput => Unit)(implicit tx: T): Unit = {
      logTxn.debug(s"write <$id>")
      store.put(_.writeInt(id))(valueFun)
    }

    def remove(id: Long)(implicit tx: T): Unit = {
      logTxn.debug(s"removL <$id>")
      store.remove(_.writeLong(id))
    }

    def remove(id: Int)(implicit tx: T): Unit = {
      logTxn.debug(s"remov <$id>")
      store.remove(_.writeInt(id))
      //         tx.markDirty()
    }

    def tryRead[A](id: Long)(valueFun: DataInput => A)(implicit tx: T): Option[A] = {
      logTxn.debug(s"readL  <$id>")
      store.get(_.writeLong(id))(valueFun)
    }

    def read[A](id: Int)(valueFun: DataInput => A)(implicit tx: T): A = {
      logTxn.debug(s"read  <$id>")
      store.get(_.writeInt(id))(valueFun).getOrElse(sys.error(s"Key not found $id"))
    }

    def exists(id: Int)(implicit tx: T): Boolean = store.contains(_.writeInt(id))

    def exists(id: Long)(implicit tx: T): Boolean = store.contains(_.writeLong(id))
  }

  trait TxnMixin[T <: D[T], I1 <: InMemoryLike.Txn[I1]]
    extends DurableLike.Txn[T] with BasicTxnImpl[T, I1] {

    self: T =>

    override type I = I1   // Dotty constraint; we have to fix the type here

    implicit def inMemoryCursor: Cursor[I] = system.inMemory

    private[lucre] final def reactionMap: ReactionMap[T] = system.reactionMap

    final def newId(): Id = new IdImpl[T](system.newIdValue()(this))
    
    final def newCachedVar[A](init: A)(implicit format: TFormat[T, A]): Var[T, A] = {
      val res = new CachedVarImpl[T, A](system.newIdValue()(this), Ref(init), format)
      res.writeInit()(this)
      res
    }

    final def newCachedIntVar(init: Int): Var[T, Int] = {
      val res = new CachedIntVar[T](system.newIdValue()(this), Ref(init))
      res.writeInit()(this)
      res
    }

    final def newCachedLongVar(init: Long): Var[T, Long] = {
      val res = new CachedLongVar[T](system.newIdValue()(this), Ref(init))
      res.writeInit()(this)
      res
    }

    final def newIdentMap[A]: IdentMap[T, A] =
      IdentMapImpl[T, A] { implicit tx => id => id.!.id }

    final def readCachedVar[A](in: DataInput)(implicit format: TFormat[T, A]): Var[T, A] = {
      val id = in./* PACKED */ readInt()
      val res = new CachedVarImpl[T, A](id, Ref.make[A](), format)
      res.readInit()(this)
      res
    }

    final def readCachedIntVar(in: DataInput): Var[T, Int] = {
      val id = in./* PACKED */ readInt()
      val res = new CachedIntVar[T](id, Ref(0))
      res.readInit()(this)
      res
    }

    final def readCachedLongVar(in: DataInput): Var[T, Long] = {
      val id = in./* PACKED */ readInt()
      val res = new CachedLongVar[T](id, Ref(0L))
      res.readInit()(this)
      res
    }

    override final def readId(in: DataInput): Id = {
      val base = in./* PACKED */ readInt()
      new IdImpl[T](base)
    }

    final def newHandle[A](value: A)(implicit format: TFormat[T, A]): Source[T, A] =
      new EphemeralSource(value)

    // ---- attributes ----

    def attrMap(obj: Obj[T]): Obj.AttrMap[T] = {
      implicit val tx : T   = this
      val mId = obj.id.!.id.toLong << 32
      val mapOpt: Option[Obj.AttrMap[T]] = system.tryRead(mId)(in => MapObj.Modifiable.read[T, String, Obj](in))
      mapOpt.getOrElse {
        val map = MapObj.Modifiable[T, String, Obj]()
        system.write(mId)(map.write)
        map
      }
    }

    override def attrMapOption(obj: Obj[T]): Option[Obj.AttrMap[T]] = {
      implicit val tx : T   = this
      val mId = obj.id.!.id.toLong << 32
      system.tryRead(mId)(MapObj.Modifiable.read[T, String, Obj](_))
    }
  }

  // XXX DRY with InMemoryImpl.IdImpl
  private final class IdImpl[T <: D[T]](val id: Int) extends DurableLike.Id[T] {
    def !(implicit tx: T): DurableLike.Id[T] = {
      // require (tx eq this.tx)
      this
    }

    def write(out: DataOutput): Unit = out./* PACKED */ writeInt(id)

    override def hashCode: Int = id

    override def equals(that: Any): Boolean = that match {
      case b: IdImpl[_] => id === b.id
      case _ => false
    }

    def dispose()(implicit tx: T): Unit = tx.system.remove(id)(tx)

    def newVar[A](init: A)(implicit tx: T, format: TFormat[T, A]): Var[T, A] = {
      val res = new VarImpl[T, A](tx.system.newIdValue(), format)
      res.setInit(init)
      res
    }

    def newBooleanVar(init: Boolean)(implicit tx: T): Var[T, Boolean] = {
      val res = new BooleanVar[T](tx.system.newIdValue())
      res.setInit(init)
      res
    }

    def newIntVar(init: Int)(implicit tx: T): Var[T, Int] = {
      val res = new IntVar[T](tx.system.newIdValue())
      res.setInit(init)
      res
    }

    def newLongVar(init: Long)(implicit tx: T): Var[T, Long] = {
      val res = new LongVar[T](tx.system.newIdValue())
      res.setInit(init)
      res
    }

    def readVar[A](in: DataInput)(implicit format: TFormat[T, A]): Var[T, A] = {
      val id = in./* PACKED */ readInt()
      new VarImpl[T, A](id, format)
    }

    def readBooleanVar(in: DataInput): Var[T, Boolean] = {
      val id = in./* PACKED */ readInt()
      new BooleanVar[T](id)
    }

    def readIntVar(in: DataInput): Var[T, Int] = {
      val id = in./* PACKED */ readInt()
      new IntVar[T](id)
    }

    def readLongVar(in: DataInput): Var[T, Long] = {
      val id = in./* PACKED */ readInt()
      new LongVar[T](id)
    }

    override def toString = s"<$id>"
  }

  private abstract class BasicVar[T <: D[T], A] extends Var[T, A] {
    protected def id: Int

    final def write(out: DataOutput): Unit = out./* PACKED */ writeInt(id)

    final def dispose()(implicit tx: T): Unit = tx.system.remove(id)(tx)

    @elidable(elidable.CONFIG) protected final def assertExists()(implicit tx: T): Unit =
      require(tx.system.exists(id)(tx), s"trying to write disposed ref $id")
  }

  private final class VarImpl[T <: D[T], A](protected val id: Int,
                                            protected val format: TFormat[T, A])
    extends BasicVar[T, A] {

    def apply()(implicit tx: T): A =
      tx.system.read[A](id)(format.readT(_)(tx))(tx)

    def setInit(v: A)(implicit tx: T): Unit =
      tx.system.write(id)(format.write(v, _))(tx)

    def update(v: A)(implicit tx: T): Unit = {
      assertExists()
      tx.system.write(id)(format.write(v, _))(tx)
    }

    def swap(v: A)(implicit tx: T): A = {
      val res = apply()
      update(v)
      res
    }

    override def toString = s"Var($id)"
  }

  private final class CachedVarImpl[T <: D[T], A](protected val id: Int, peer: Ref[A],
                                                  format: TFormat[T, A])
    extends BasicVar[T, A] {

    def apply()(implicit tx: T): A = peer.get(tx.peer)

//    def setInit(v: A)(implicit tx: T): Unit = this() = v

    def update(v: A)(implicit tx: T): Unit = {
      peer.set(v)(tx.peer)
      tx.system.write(id)(format.write(v, _))(tx)
    }

    def writeInit()(implicit tx: T): Unit =
      tx.system.write(id)(format.write(this(), _))(tx)

    def readInit()(implicit tx: T): Unit =
      peer.set(tx.system.read(id)(format.readT(_)(tx))(tx))(tx.peer)

    def swap(v: A)(implicit tx: T): A = {
      val res = peer.swap(v)(tx.peer)
      tx.system.write(id)(format.write(v, _))(tx)
      res
    }

    override def toString = s"Var($id)"
  }

  private final class BooleanVar[T <: D[T]](protected val id: Int)
    extends BasicVar[T, Boolean] {

    def apply()(implicit tx: T): Boolean =
      tx.system.read[Boolean](id)(_.readBoolean())(tx)

    def setInit(v: Boolean)(implicit tx: T): Unit =
      tx.system.write(id)(_.writeBoolean(v))(tx)

    def update(v: Boolean)(implicit tx: T): Unit = {
      assertExists()
      tx.system.write(id)(_.writeBoolean(v))(tx)
    }

    def swap(v: Boolean)(implicit tx: T): Boolean = {
      val res = apply()
      update(v)
      res
    }

    override def toString = s"Var[Boolean]($id)"
  }

  private final class IntVar[T <: D[T]](protected val id: Int)
    extends BasicVar[T, Int] {

    def apply()(implicit tx: T): Int =
      tx.system.read[Int](id)(_.readInt())(tx)

    def setInit(v: Int)(implicit tx: T): Unit =
      tx.system.write(id)(_.writeInt(v))(tx)

    def update(v: Int)(implicit tx: T): Unit = {
      assertExists()
      tx.system.write(id)(_.writeInt(v))(tx)
    }

    def swap(v: Int)(implicit tx: T): Int = {
      val res = apply()
      update(v)
      res
    }

    override def toString = s"Var[Int]($id)"
  }

  private final class CachedIntVar[T <: D[T]](protected val id: Int, peer: Ref[Int])
    extends BasicVar[T, Int] {

    def apply()(implicit tx: T): Int = peer.get(tx.peer)

//    def setInit(v: Int)(implicit tx: T): Unit = this() = v

    def update(v: Int)(implicit tx: T): Unit = {
      peer.set(v)(tx.peer)
      tx.system.write(id)(_.writeInt(v))(tx)
    }

    def writeInit()(implicit tx: T): Unit =
      tx.system.write(id)(_.writeInt(this()))(tx)

    def readInit()(implicit tx: T): Unit =
      peer.set(tx.system.read(id)(_.readInt())(tx))(tx.peer)

    def swap(v: Int)(implicit tx: T): Int = {
      val res = peer.swap(v)(tx.peer)
      tx.system.write(id)(_.writeInt(v))(tx)
      res
    }

    override def toString = s"Var[Int]($id)"
  }

  private final class LongVar[T <: D[T]](protected val id: Int)
    extends BasicVar[T, Long] {

    def apply()(implicit tx: T): Long =
      tx.system.read[Long](id)(_.readLong())(tx)

    def setInit(v: Long)(implicit tx: T): Unit =
      tx.system.write(id)(_.writeLong(v))(tx)

    def update(v: Long)(implicit tx: T): Unit = {
      assertExists()
      tx.system.write(id)(_.writeLong(v))(tx)
    }

    def swap(v: Long)(implicit tx: T): Long = {
      val res = this()
      this() = v
      res
    }

    override def toString = s"Var[Long]($id)"
  }

  private final class CachedLongVar[T <: D[T]](protected val id: Int, peer: Ref[Long])
    extends BasicVar[T, Long] {

    def apply()(implicit tx: T): Long = peer.get(tx.peer)

//    def setInit(v: Long)(implicit tx: T): Unit = this() = v

    def update(v: Long)(implicit tx: T): Unit = {
      peer.set(v)(tx.peer)
      tx.system.write(id)(_.writeLong(v))(tx)
    }

    def writeInit()(implicit tx: T): Unit =
      tx.system.write(id)(_.writeLong(this()))(tx)

    def readInit()(implicit tx: T): Unit =
      peer.set(tx.system.read(id)(_.readLong())(tx))(tx.peer)

    def swap(v: Long)(implicit tx: T): Long = {
      val res = peer.swap(v)(tx.peer)
      tx.system.write(id)(_.writeLong(v))(tx)
      res
    }

    override def toString = s"Var[Long]($id)"
  }

  private final class TxnImpl(val system: System, val peer: InTxn)
    extends TxnMixin[Durable.Txn, InMemory.Txn] with Durable.Txn {

    lazy val inMemory: InMemory.Txn = system.inMemory.wrap(peer)

    def inMemoryBridge: (Durable.Txn => InMemory.Txn) = _.inMemory

    override def toString = s"Durable.Txn@${hashCode.toHexString}"
  }

  private final class System(val store: DataStore)
    extends Mixin[Durable.Txn, InMemory.Txn] with Durable {

//    private type S = Durable    // scalac bug -- it _is_ used

    val inMemory: InMemory = InMemory()

//    def inMemoryTx(tx: Tx): I#Tx = tx.inMemory

    override def toString = s"Durable@${hashCode.toHexString}"

    def wrap(peer: InTxn, systemTimeNanos: Long): T = new TxnImpl(this, peer)
  }
}
