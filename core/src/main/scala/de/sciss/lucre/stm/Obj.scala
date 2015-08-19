/*
 *  Obj.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import de.sciss.lucre.event.Event
import de.sciss.lucre.stm
import de.sciss.serial.{Serializer, DataInput}
import impl.{ObjImpl => Impl}

object Obj {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Obj[S]] = Impl.serializer

  trait Type {
    def typeID: Int

    private[this] lazy val _init: Unit = Obj.register(this)

    final def init(): Unit = _init

    final def readObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
      val tpe = in.readInt()
      if (tpe != typeID) sys.error(s"Type mismatch, expected $typeID but found $tpe")
      readIdentifiedObj(in, access)
    }

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S]
  }

  def register(tpe: Type): Unit = Impl.register(tpe)
}
trait Obj[S <: Sys[S]] extends stm.Mutable[S#ID, S#Tx] {
  override def toString = s"Obj$id"

  def typeID: Int

  private[lucre] def event(slot: Int): Event[S, Any]
}