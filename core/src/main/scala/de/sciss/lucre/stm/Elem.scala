/*
 *  Elem.scala
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

import de.sciss.lucre.event.{Publisher, Event}
import de.sciss.lucre.stm.impl.{ElemImpl => Impl}
import de.sciss.serial
import de.sciss.serial.{DataInput, Writable}

object Elem {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Elem[S]] = Impl.serializer

  trait Type {
    def typeId: Int

    private[this] lazy val _init: Unit = Elem.addType(this)

    def init(): Unit = _init

    def readObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = {
      val tpe = in.readInt()
      if (tpe != typeId) sys.error(s"Type mismatch, expected $typeId but found $tpe")
      readIdentifiedObj(in, access)
    }

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S]
  }

  def addType(tpe: Type): Unit      = Impl.addType(tpe)
  def getType(id : Int ): Elem.Type = Impl.getType(id )
}

/** An `Elem` is any type that is globally registered
  * via `Elem.addType` and can be de-serialized through `Elem.read`.
  */
trait Elem[S <: Sys[S]] extends Writable with Disposable[S#Tx] with Publisher[S, Any] {
  def tpe: Elem.Type

  /** Selects an event during dispatch. Elements that do not provide events
    * should simply throw an exception. They will in any case not be encountered during dispatch.
    */
  private[lucre] def event(slot: Int): Event[S, Any]

  /** Makes a deep copy of an element, possibly translating it to a different system `Out`. */
  private[lucre] def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out]
}