package de.sciss.lucre.stm.impl

import de.sciss.lucre.event.Event
import de.sciss.lucre.stm.{Elem, Sys}
import de.sciss.serial.DataOutput

trait ConstElemImpl[S <: Sys[S]] extends Elem[S] {
  private[lucre] def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

  final def write(out: DataOutput): Unit = {
    out.writeInt(tpe.typeID)
    out.writeByte(3)
    writeData(out)
  }

  final def dispose()(implicit tx: S#Tx) = ()

  protected def writeData(out: DataOutput): Unit
}