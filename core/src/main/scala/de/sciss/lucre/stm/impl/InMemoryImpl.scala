/*
 *  InMemoryImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm.impl

import de.sciss.lucre.event.impl.ReactionMapImpl
import de.sciss.lucre.event.{Observer, ReactionMap}
import de.sciss.lucre.stm.InMemoryLike.Var
import de.sciss.lucre.stm.{IdentifierMap, InMemory, InMemoryLike, Source, TxnLike}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.concurrent.stm.{InTxn, Ref => ScalaRef, TMap, TxnExecutor}

object InMemoryImpl {
  private type S = InMemory

  def apply(): InMemory = new System

  trait Mixin[S <: InMemoryLike[S]] extends InMemoryLike[S] with ReactionMapImpl.Mixin[S] {
    private final val idCnt = ScalaRef(0)

    final protected val eventMap: IdentifierMap[S#ID, S#Tx, Map[Int, List[Observer[S, _]]]] =
      IdentifierMap.newInMemoryIntMap[S#ID, S#Tx, Map[Int, List[Observer[S, _]]]](_.id)

    //    final def newID(peer: InTxn): S#ID = {
//      // // since idCnt is a ScalaRef and not InMemory#Var, make sure we don't forget to mark the txn dirty!
//      // dirty = true
//      val res = idCnt.get(peer) + 1
//      idCnt.set(res)(peer)
//      new IDImpl[S](res)
//    }

    final private[lucre] def newIDValue()(implicit tx: S#Tx): Int = {
      val peer  = tx.peer
      val res   = idCnt.get(peer) + 1
      idCnt.set(res)(peer)
      res
    }

    final def root[A](init: S#Tx => A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
      step { implicit tx =>
        tx.newVar[A](tx.newID(), init(tx))
      }

    // may nest
    def rootJoin[A](init: S#Tx => A)(implicit tx: TxnLike, serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
      root(init)

    final def close(): Unit = ()

    // ---- cursor ----

    final def step[A](fun: S#Tx => A): A = {
      // note: in-memory has no problem with nested
      // transactions, so we do not need to check that condition.
      TxnExecutor.defaultAtomic(itx => fun(wrap(itx))) // new TxnImpl( this, itx )))
    }

    final def position(implicit tx: S#Tx): S#Acc = ()
  }

  private def opNotSupported(name: String): Nothing = sys.error(s"Operation not supported: $name")

  private final class VarImpl[S <: InMemoryLike[S], A](val peer: ScalaRef[A])
    extends InMemoryLike.Var[S, A] {

    override def toString = s"Var<${hashCode().toHexString}>"

//    def apply()     (implicit tx: S#Tx): A    = tx.getVar(this)
//    def update(v: A)(implicit tx: S#Tx): Unit = tx.putVar(this, v)

    def apply()     (implicit tx: S#Tx): A    = peer.get   (tx.peer)
    def update(v: A)(implicit tx: S#Tx): Unit = peer.set(v)(tx.peer)

    //    def transform(f: A => A)(implicit tx: S#Tx): Unit = {
//      peer.transform(f)(tx.peer)
//      // tx.markDirty()
//    }

    def write(out: DataOutput): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      // XXX TODO --- what to do with the contexts?
      peer.set(null.asInstanceOf[A])(tx.peer)
      // tx.markDirty()
    }
  }

  private final class IDImpl[S <: InMemoryLike[S]](val id: Int) extends InMemoryLike.ID[S] {
    def write(out: DataOutput): Unit = ()
    def dispose()(implicit tx: S#Tx): Unit = ()

    override def toString = s"<$id>"
    override def hashCode: Int = id.##

    override def equals(that: Any) = that match {
      case thatID: InMemoryLike.ID[_] => thatID.id == id
      case _ => false
    }
  }

  private final class ContextImpl[S <: InMemoryLike[S]] extends InMemoryLike.Context[S] {
    private val map = TMap.empty[Any, Any]

    def get[A](vr: Var[S, A])(implicit tx: S#Tx): A = {
      import TxnLike.peer
      map.getOrElse(vr, vr.peer.get).asInstanceOf[A]
    }

    def put[A](vr: Var[S, A], value: A)(implicit tx: S#Tx): Unit = map.put(vr, value)(tx.peer)
  }

  private final class TxnImpl(val system: InMemory, val peer: InTxn)
    extends TxnMixin[InMemory] {

    implicit def inMemory = this

    override def toString = s"InMemory.Txn@${hashCode.toHexString}"
  }

  trait TxnMixin[S <: InMemoryLike[S]] extends BasicTxnImpl[S] with InMemoryLike.Txn[S] {
    _: S#Tx =>

    final def newID(): S#ID = new IDImpl(system.newIDValue()(this))

//    final def newPartialID(): S#ID = newID()

    final def newHandle[A](value: A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S#Tx, A] =
      new EphemeralHandle(value)

    private[stm] def getVar[A](vr: Var[S, A]): A = {
      if (_context == null) vr.peer.get(peer)
      else _context.get(vr)(this)
    }

    private[stm] def putVar[A](vr: Var[S, A], value: A): Unit = {
      if (_context == null) vr.peer.set(value)(peer)
      else _context.put(vr, value)(this)
    }

    final def newVar[A](id: S#ID, init: A)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] = {
      val peer = ScalaRef(init)
      new VarImpl(peer)
    }

//    final def newLocalVar[A](init: S#Tx => A): LocalVar[S#Tx, A] = new impl.LocalVarImpl[S, A](init)
//
//    final def newPartialVar[A](id: S#ID, init: A)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] =
//      newVar(id, init)

    final def newIntVar(id: S#ID, init: Int): S#Var[Int] = {
      val peer = ScalaRef(init)
      new VarImpl(peer)
    }

    final def newBooleanVar(id: S#ID, init: Boolean): S#Var[Boolean] = {
      val peer = ScalaRef(init)
      new VarImpl(peer)
    }

    final def newLongVar(id: S#ID, init: Long): S#Var[Long] = {
      val peer = ScalaRef(init)
      new VarImpl(peer)
    }

    final def newVarArray[A](size: Int) = new Array[S#Var[A]](size)

    final def newInMemoryIDMap[A]: IdentifierMap[S#ID, S#Tx, A] =
      IdentifierMapImpl.newInMemoryIntMap[S#ID, S#Tx, A](_.id)

//    final def newDurableIDMap[A](implicit serializer: Serializer[S#Tx, S#Acc, A]): IdentifierMap[S#ID, S#Tx, A] =
//      IdentifierMap.newInMemoryIntMap[S#ID, S#Tx, A](new IDImpl(0))(_.id)

    def readVar[A](id: S#ID, in: DataInput)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] =
      opNotSupported("readVar")

//    def readPartialVar[A](pid: S#ID, in: DataInput)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] =
//      readVar(pid, in)

    def readBooleanVar(id: S#ID, in: DataInput): S#Var[Boolean] = opNotSupported("readBooleanVar")
    def readIntVar    (id: S#ID, in: DataInput): S#Var[Int    ] = opNotSupported("readIntVar"    )
    def readLongVar   (id: S#ID, in: DataInput): S#Var[Long   ] = opNotSupported("readLongVar"   )

    def readID(in: DataInput, acc: S#Acc): S#ID = opNotSupported("readID")

//    def readPartialID(in: DataInput, acc: S#Acc): S#ID = readID(in, acc)

//    def readDurableIDMap[A](in: DataInput)
//                           (implicit serializer: Serializer[S#Tx, S#Acc, A]): IdentifierMap[S#ID, S#Tx, A] =
//      opNotSupported("readDurableIDMap")

    final private[lucre] def reactionMap: ReactionMap[S] = system.reactionMap

    def newContext(): S#Context = new ContextImpl[S]
  }

  private final class System extends Mixin[InMemory] with InMemory {
    private type S = InMemory

    def inMemory: I = this
    def inMemoryTx(tx: Tx): Tx = tx

    override def toString = s"InMemory@${hashCode.toHexString}"

    def wrap(itx: InTxn): S#Tx = new TxnImpl(this, itx)
  }
}