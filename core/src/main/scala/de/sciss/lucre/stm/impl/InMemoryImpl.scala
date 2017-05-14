/*
 *  InMemoryImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2017 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.stm.{IdentifierMap, InMemory, InMemoryLike, Obj, Source, TxnLike}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.concurrent.stm.{InTxn, Ref => ScalaRef, TxnExecutor}
import scala.language.higherKinds

object InMemoryImpl {
  def apply(): InMemory = new System

  trait Mixin[S <: InMemoryLike[S]] extends InMemoryLike[S] with ReactionMapImpl.Mixin[S] {
    private[this] final val idCnt = ScalaRef(0)

    protected final val eventMap: IdentifierMap[S#ID, S#Tx, Map[Int, List[Observer[S, _]]]] =
      IdentifierMap.newInMemoryIntMap[S#ID, S#Tx, Map[Int, List[Observer[S, _]]]](_.id)

    private[lucre] final val attrMap: IdentifierMap[S#ID, S#Tx, Obj.AttrMap[S]] =
      IdentifierMap.newInMemoryIntMap[S#ID, S#Tx, Obj.AttrMap[S]](_.id)

    private[lucre] final def newIDValue()(implicit tx: S#Tx): Int = {
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
    override def hashCode: Int    = id.##

    override def equals(that: Any): Boolean = that match {
      case thatID: InMemoryLike.ID[_] => thatID.id == id
      case _ => false
    }
  }

//  private final class ContextImpl[S <: InMemoryLike[S]] extends InMemoryLike.Context[S] {
//    private val map = TMap.empty[Any, Any]
//
//    def get[A](vr: Var[S, A])(implicit tx: S#Tx): A = {
//      import TxnLike.peer
//      map.getOrElse(vr, vr.peer.get).asInstanceOf[A]
//    }
//
//    def put[A](vr: Var[S, A], value: A)(implicit tx: S#Tx): Unit = map.put(vr, value)(tx.peer)
//  }

  private final class TxnImpl(val system: InMemory, val peer: InTxn)
    extends TxnMixin[InMemory] {

    implicit def inMemory: InMemory#I#Tx = this

    override def toString = s"InMemory.Txn@${hashCode.toHexString}"
  }

  trait TxnMixin[S <: InMemoryLike[S]] extends BasicTxnImpl[S] with InMemoryLike.Txn[S] {
    _: S#Tx =>

    final def newID(): S#ID = new IDImpl[S](system.newIDValue()(this))

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
      new VarImpl[S, A](peer)
    }

    final def newIntVar(id: S#ID, init: Int): S#Var[Int] = {
      val peer = ScalaRef(init)
      new VarImpl[S, Int](peer)
    }

    final def newBooleanVar(id: S#ID, init: Boolean): S#Var[Boolean] = {
      val peer = ScalaRef(init)
      new VarImpl[S, Boolean](peer)
    }

    final def newLongVar(id: S#ID, init: Long): S#Var[Long] = {
      val peer = ScalaRef(init)
      new VarImpl[S, Long](peer)
    }

    final def newVarArray[A](size: Int) = new Array[S#Var[A]](size)

    final def newInMemoryIDMap[A]: IdentifierMap[S#ID, S#Tx, A] =
      IdentifierMapImpl.newInMemoryIntMap[S#ID, S#Tx, A](_.id)

    def readVar[A](id: S#ID, in: DataInput)(implicit ser: Serializer[S#Tx, S#Acc, A]): S#Var[A] =
      opNotSupported("readVar")

    def readBooleanVar(id: S#ID, in: DataInput): S#Var[Boolean] = opNotSupported("readBooleanVar")
    def readIntVar    (id: S#ID, in: DataInput): S#Var[Int    ] = opNotSupported("readIntVar"    )
    def readLongVar   (id: S#ID, in: DataInput): S#Var[Long   ] = opNotSupported("readLongVar"   )

    def readID(in: DataInput, acc: S#Acc): S#ID = opNotSupported("readID")

    private[lucre] final def reactionMap: ReactionMap[S] = system.reactionMap

    // ---- context ----

    // def newContext(): S#Context = new ContextImpl[S]

    // ---- attributes ----

    def attrMap(obj: Obj[S]): Obj.AttrMap[S] = {
      implicit val tx = this
      val am  = system.attrMap
      val id  = obj.id
      am.getOrElse(id, {
        val m = evt.Map.Modifiable[S, String, Obj]
        am.put(id, m)
        m
      })
    }

//    def attrGet(obj: Obj[S], key: String): Option[Obj[S]] =
//      system.attrMap.getOrElse(obj.id, Map.empty)(this).get(key)
//
//    def attrPut(obj: Obj[S], key: String, value: Obj[S]): Unit = {
//      val a    = system.attrMap
//      val id   = obj.id
//      val map0 = a.getOrElse(id, Map.empty)(this)
//      val map1 = map0 + (key -> value)
//      a.put(id, map1)(this)
//    }
//
//    def attrRemove(obj: Obj[S], key: String): Unit = {
//      val a    = system.attrMap
//      val id   = obj.id
//      val map0 = a.getOrElse(id, Map.empty)(this)
//      if (map0.nonEmpty) {
//        val map1 = map0 - key
//        if (map1.isEmpty)
//          a.remove(id)(this)
//        else
//          a.put(id, map1)(this)
//      }
//    }
//
//    def attrIterator(obj: Obj[S]): Iterator[(String, Obj[S])] =
//      system.attrMap.get(obj.id)(this).fold[Iterator[(String, Obj[S])]](Iterator.empty)(_.iterator)
  }

  private final class System extends Mixin[InMemory] with InMemory {
    private type S = InMemory

    def inMemory: I = this
    def inMemoryTx(tx: Tx): Tx = tx

    override def toString = s"InMemory@${hashCode.toHexString}"

    def wrap(itx: InTxn): S#Tx = new TxnImpl(this, itx)
  }
}