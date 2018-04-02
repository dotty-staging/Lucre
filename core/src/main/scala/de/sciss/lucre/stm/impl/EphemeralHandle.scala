/*
 *  EphemeralHandle.scala
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

package de.sciss.lucre.stm.impl

import de.sciss.lucre.stm.Source

final class EphemeralHandle[Tx, A](value: A) extends Source[Tx, A] {
  override def toString = s"handle: $value"

  def apply()(implicit tx: Tx): A = value
}