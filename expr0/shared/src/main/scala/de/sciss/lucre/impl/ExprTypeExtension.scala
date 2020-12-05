/*
 *  ExprTypeExtension.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.Txn
import de.sciss.serial.{DataInput, DataOutput, TFormat, WritableFormat}

trait ExprTypeExtension {
  def name: String

  /** Lowest id of handled operators */
  val opLo : Int
  /** Highest id of handled operators. Note: This value is _inclusive_ */
  val opHi : Int

  override def toString = s"$name [lo = $opLo, hi = $opHi]"
}

trait ExprTypeExtension1[Repr[~ <: Txn[~]]] extends ExprTypeExtension {
  def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                (implicit tx: T): Repr[T]
}

trait ExprTypeExtension2[Repr[~ <: Txn[~], _]] extends ExprTypeExtension {
  def readExtension[T <: Txn[T], T1](opId: Int, in: DataInput, targets: Targets[T])
                                    (implicit tx: T): Repr[T, T1]
}

trait ExprTypeExtension3[Repr[~ <: Txn[~], _, _]] extends ExprTypeExtension {
  def readExtension[T <: Txn[T], T1, T2](opId: Int, in: DataInput, targets: Targets[T])
                                        (implicit tx: T): Repr[T, T1, T2]
}
