/*
 *  MutableSerializer.scala
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

import de.sciss.serial.{DataInput, DataOutput, Serializer}

trait MutableSerializer[S <: Sys[S], M <: Mutable[S#ID, S#Tx]]
  extends Serializer[S#Tx, S#Acc, M] {

  final def write(m: M, out: DataOutput): Unit = m.write(out)

  final def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): M = {
    val id = tx.readID(in, access)
    readData(in, id)
  }

  protected def readData(in: DataInput, id: S#ID)(implicit tx: S#Tx): M
}