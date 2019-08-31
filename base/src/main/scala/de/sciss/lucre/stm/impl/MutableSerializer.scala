/*
 *  MutableSerializer.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm
package impl

import de.sciss.serial.{DataInput, DataOutput, Serializer}

trait MutableSerializer[S <: Base[S], M <: Mutable[S#Id, S#Tx]]
  extends Serializer[S#Tx, S#Acc, M] {

  final def write(m: M, out: DataOutput): Unit = m.write(out)

  final def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): M = {
    val id = tx.readId(in, access)
    readData(in, id)
  }

  protected def readData(in: DataInput, id: S#Id)(implicit tx: S#Tx): M
}