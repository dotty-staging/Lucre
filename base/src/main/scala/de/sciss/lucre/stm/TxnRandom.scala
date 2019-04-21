/*
 *  TxnRandom.scala
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

package de.sciss.lucre.stm

import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.RandomImpl.{SysLike, calcSeedUniquifier, initialScramble}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object TxnRandom {
   private final class Impl[S <: Base[S]](val id: S#Id, protected val seedRef: stm.Var[S#Tx, Long])
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

  def apply[S <: Base[S]]()(implicit tx: S#Tx): TxnRandom[S] =
    apply(calcSeedUniquifier() ^ System.nanoTime())

  def apply[S <: Base[S]](seed: Long)(implicit tx: S#Tx): TxnRandom[S] = {
    val id = tx.newId()
    new Impl[S](id, tx.newLongVar(id, initialScramble(seed)))
  }

  def read[S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): TxnRandom[S] =
    serializer[S].read(in, access)

  implicit def serializer[S <: Base[S]]: Serializer[S#Tx, S#Acc, TxnRandom[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[Plain]

  private final class Ser[S <: Base[S]] extends Serializer[S#Tx, S#Acc, TxnRandom[S]] {
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
trait TxnRandom[S <: Base[S]] extends Random[S#Tx] with stm.Mutable[S#Id, S#Tx]