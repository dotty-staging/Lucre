/*
 *  ImmutableReader.scala
 *  (Serial)
 *
 * Copyright (c) 2011-2018 Hanns Holger Rutz. All rights reserved.
 *
 * This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 * For further information, please contact Hanns Holger Rutz at
 * contact@sciss.de
 */

package de.sciss
package serial

import de.sciss.lucre.stm.Tx

trait ImmutableReader[+A] extends Reader[Tx, A] {
  def read(in: DataInput): A

  final def read(in: DataInput, tx: Tx)(implicit access: tx.Acc): A = read(in)
}
