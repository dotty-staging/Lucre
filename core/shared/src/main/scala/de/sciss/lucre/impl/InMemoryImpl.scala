/*
 *  InMemoryImpl.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.serial.{DataInput, DataOutput, TFormat}

import scala.concurrent.stm.{InTxn, TxnExecutor, Ref => ScalaRef}

object InMemoryImpl {
  def apply(): InMemory = new System

  trait Mixin[T <: InMemoryLike.Txn[T]] extends InMemoryLike[T] with ReactionMapImpl.Mixin[T] {
    private[this] final val idCnt = ScalaRef(0)

    //    protected val idIntView: T => Ident[]

    protected final val eventMap: IdentMap[T, Map[Int, scala.List[Observer[T, _]]]] =
      IdentMapImpl[T, Map[Int, scala.List[Observer[T, _]]]] { implicit tx => id => id.!.id }

    private[lucre] final val attrMap: IdentMap[T, Obj.AttrMap[T]] =
      IdentMapImpl[T, Obj.AttrMap[T]] { implicit tx => id => id.!.id }

    private[lucre] final def newIdValue()(implicit tx: T): Int = {
      val peer  = tx.peer
      val res   = idCnt.get(peer) + 1
      idCnt.set(res)(peer)
      res
    }

    final def root[A](init: T => A)(implicit format: TFormat[T, A]): Source[T, A] =
      step { implicit tx =>
//        val id  = tx.newId()
        val v   = init(tx)
//        id.newVar[A](v)
        tx.newHandle(v) // new EphemeralHandle(v)
      }

    // may nest
    def rootJoin[A](init: T => A)(implicit tx: TxnLike, format: TFormat[T, A]): Source[T, A] =
      root(init)

    final def close(): Unit = ()

    // ---- cursor ----

    final def step[A](fun: T => A): A = stepTag(0L)(fun)

    final def stepTag[A](systemTimeNanos: Long)(fun: T => A): A = {
      // note: in-memory has no problem with nested
      // transactions, so we do not need to check that condition.
      TxnExecutor.defaultAtomic(itx => fun(wrap(itx, systemTimeNanos)))
    }

//    final def position(implicit tx: T): Unit = ()
  }

  private def opNotSupported(name: String): Nothing = sys.error(s"Operation not supported: $name")

  private final class IdImpl[T <: InMemoryLike.Txn[T]](/*tx: T*/)(val id: Int) extends InMemoryLike.Id[T] {
    def write(out: DataOutput): Unit = ()

    override def toString         = s"<$id>"
    override def hashCode: Int    = id.##

    def dispose()(implicit tx: T): Unit = ()

    def !(implicit tx: T): InMemoryLike.Id[T] = {
      // require (tx eq this.tx)
      this
    }

    def newVar[A](init: A)(implicit tx: T, format: TFormat[T, A]): Var[T, A] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[A](peer)
    }

    def newBooleanVar(init: Boolean)(implicit tx: T): Var[T, Boolean] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[Boolean](peer)
    }

    def newIntVar(init: Int)(implicit tx: T): Var[T, Int] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[Int](peer)
    }

    def newLongVar(init: Long)(implicit tx: T): Var[T, Long] = {
      val peer = ScalaRef(init)
      new SysInMemoryRef[Long](peer)
    }

    def readVar[A](in: DataInput)(implicit format: TFormat[T, A]): Var[T, A] =
      opNotSupported("readVar")

    def readBooleanVar(in: DataInput): Var[T, Boolean] =
      opNotSupported("readBooleanVar")

    def readIntVar(in: DataInput): Var[T, Int] =
      opNotSupported("readIntVar")

    def readLongVar(in: DataInput): Var[T, Long] =
      opNotSupported("readLongVar")

    override def equals(that: Any): Boolean = that match {
      case thatId: InMemoryLike.Id[_] => thatId.id === id
      case _ => false
    }
  }

  private final class TxnImpl(val system: InMemory, val peer: InTxn)
    extends TxnMixin[InMemory.Txn] with InMemory.Txn {

    def inMemory: InMemory.Txn = this

    def inMemoryBridge: InMemory.Txn => InMemory.Txn = tx => tx

    override def toString = s"InMemory.Txn@${hashCode.toHexString}"
  }

  trait TxnMixin[T <: InMemoryLike.Txn[T]] extends BasicTxnImpl[T, T] with InMemoryLike.Txn[T] {
    self: T =>

    implicit def inMemoryCursor: Cursor[I] = system

    final def newId(): Id = new IdImpl[T](/*this*/)(system.newIdValue()(this))

    final def newHandle[A](value: A)(implicit format: TFormat[T, A]): Source[T, A] =
      new EphemeralSource(value)

    private[lucre] def getVar[A](vr: Var[A]): A = {
      vr.peer.get(peer)
    }

    private[lucre] def putVar[A](vr: Var[A], value: A): Unit = {
      vr.peer.set(value)(peer)
    }

    final def newVarArray[A](size: Int) = new Array[Var[A]](size)

    final def newIdentMap[A]: IdentMap[T, A] =
      IdentMapImpl[T, A] { implicit tx => id => id.!.id }

    override def readId(in: DataInput): Id = opNotSupported("readId")

    private[lucre] final def reactionMap: ReactionMap[T] = system.reactionMap

    // ---- attributes ----

    def attrMap(obj: Obj[T]): Obj.AttrMap[T] = {
      implicit val tx: T = this
      val am  = system.attrMap
      val id  = obj.id.!
      am.getOrElse(id, {
        val m = MapObj.Modifiable[T, String, Obj]()
        am.put(id, m)
        m
      })
    }

    override def attrMapOption(obj: Obj[T]): Option[Obj.AttrMap[T]] = {
      implicit val tx: T = this
      val am  = system.attrMap
      val id  = obj.id.!
      am.get(id)
    }
  }

  private final class System extends Mixin[InMemory.Txn] with InMemory {
//    def inMemory: I = this
//    def inMemoryTx(tx: T): T = tx

//    def inMemoryBridge: (T => T) = tx => tx

    override def toString = s"InMemory@${hashCode.toHexString}"

    def wrap(itx: InTxn, systemTimeNanos: Long): T = new TxnImpl(this, itx)
  }
}