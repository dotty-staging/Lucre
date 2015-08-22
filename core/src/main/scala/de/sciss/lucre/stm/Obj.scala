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

import de.sciss.lucre.event.{Event, Targets}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.{ObjImpl => Impl}
import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput}

object Obj {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Obj[S]] = Impl.serializer

  trait Type extends Elem.Type {
    private[this] lazy val _init: Unit = Obj.addType(this)

    override def init(): Unit = {
      super.init()
      _init
    }

    final override def readObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
      val tpe = in.readInt()
      if (tpe != typeID) sys.error(s"Type mismatch, expected $typeID but found $tpe")
      readIdentifiedObj(in, access)
    }

    override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S]
  }

  def addType(tpe: Type): Unit      = Impl.addType(tpe)
  def getType(id : Int ): Obj.Type  = Impl.getType(id )
}

/** An `Obj` is a type of element that has an `S#ID` identifier and
  * an attribute map. It can be the origin of event dispatch.
  */
trait Obj[S <: Sys[S]] extends Elem[S] with stm.Mutable[S#ID, S#Tx] {
  override def toString = s"Obj$id"

  private[lucre] def event(slot: Int): Event[S, Any]
}