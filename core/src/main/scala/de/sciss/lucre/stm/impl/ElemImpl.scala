/*
 *  ElemImpl.scala
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
package impl

import de.sciss.serial.{DataOutput, DataInput, Serializer}

import scala.annotation.meta.field

object ElemImpl {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = {
    val typeID  = in.readInt()
    val tpe     = getType(typeID)
    tpe.readIdentifiedObj(in, access)
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Elem[S]] = anySer.asInstanceOf[Ser[S]]

  @field private[this] final val sync   = new AnyRef
  @field private[this] final val anySer = new Ser[NoSys]

  @volatile private var map = Map.empty[Int, Elem.Type]

  def addType(tpe: Elem.Type): Unit = sync.synchronized {
    val typeID = tpe.typeID
    if (map.contains(typeID))
      throw new IllegalArgumentException(s"Element type $typeID was already registered ($tpe overrides ${map(typeID)})")

    map += typeID -> tpe
  }

  @inline
  def getType(id: Int): Elem.Type = map.getOrElse(id, sys.error(s"Unknown element type $id"))

  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Elem[S]] {
    def write(obj: Elem[S], out: DataOutput): Unit = obj.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = ElemImpl.read(in, access)
  }
}