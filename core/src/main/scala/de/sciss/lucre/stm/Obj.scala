package de.sciss.lucre.stm

import de.sciss.lucre.event.Event
import de.sciss.lucre.stm
import de.sciss.serial.{Serializer, DataInput}

object Obj {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = ???

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Obj[S]] = ???
}
trait Obj[S <: Sys[S]] extends stm.Mutable[S#ID, S#Tx] {
  override def toString = s"Obj$id"

  def typeID: Int

  private[lucre] def select(slot: Int): Event[S, Any]
}