/*
 *  ConstElemImpl.scala
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

import de.sciss.lucre.event.Event
import de.sciss.lucre.stm.{Elem, Sys}
import de.sciss.serial.DataOutput

trait ConstElemImpl[S <: Sys[S]] extends Elem[S] {
  private[lucre] def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

  final def write(out: DataOutput): Unit = {
    out.writeInt(tpe.typeId)
    out.writeByte(3)
    writeData(out)
  }

  final def dispose()(implicit tx: S#Tx): Unit = ()

  protected def writeData(out: DataOutput): Unit
}