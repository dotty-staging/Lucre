/*
 *  IdentifierSerializer.scala
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
package impl

import de.sciss.serial.{DataInput, DataOutput, Serializer}

final class IdentifierSerializer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, S#ID] {
  def write(id: S#ID, out: DataOutput): Unit = id.write(out)

  def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): S#ID = tx.readID(in, access)
}
