/*
 *  TxnRandom.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.RandomImpl.{SysLike, calcSeedUniquifier, initialScramble, BaseImpl}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.concurrent.stm.{InTxn, Ref => STMRef}

object TxnRandom {
  def peer():           Random[InTxn] = peer(calcSeedUniquifier() ^ System.nanoTime())
  def peer(seed: Long): Random[InTxn] = new PlainImpl(STMRef(initialScramble(seed)))

  private final class PlainImpl(seedRef: STMRef[Long]) extends BaseImpl[InTxn] {
    protected def refSet(value: Long)(implicit tx: InTxn): Unit = seedRef() = value

    protected def refGet(implicit tx: InTxn): Long = seedRef()
  }

  private final class Impl[S <: stm.Sys[S]](val id: S#Id, protected val seedRef: stm.Var[S#Tx, Long])
    extends SysLike[S#Tx] with TxnRandom[S] {

    def write(out: DataOutput): Unit = {
      id     .write(out)
      seedRef.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id     .dispose()
      seedRef.dispose()
    }
  }

  def apply[S <: stm.Sys[S]]()(implicit tx: S#Tx): TxnRandom[S] =
    apply(calcSeedUniquifier() ^ System.nanoTime())

  def apply[S <: stm.Sys[S]](seed: Long)(implicit tx: S#Tx): TxnRandom[S] = {
    val id = tx.newId()
    new Impl[S](id, tx.newLongVar(id, initialScramble(seed)))
  }

  def read[S <: stm.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): TxnRandom[S] =
    serializer[S].read(in, access)

  implicit def serializer[S <: stm.Sys[S]]: Serializer[S#Tx, S#Acc, TxnRandom[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: stm.Sys[S]] extends Serializer[S#Tx, S#Acc, TxnRandom[S]] {
    def write(p: TxnRandom[S], out: DataOutput): Unit = p.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): TxnRandom[S] = {
      val id      = tx.readId(in, access)
      val seedRef = tx.readVar[Long](id, in)
      new Impl(id, seedRef)
    }
  }
}

/** A transactional pseudo-random number generator which
  * behaves numerically like `java.util.Random`.
  */
trait TxnRandom[S <: stm.Sys[S]] extends Random[S#Tx] with stm.Mutable[S#Id, S#Tx]