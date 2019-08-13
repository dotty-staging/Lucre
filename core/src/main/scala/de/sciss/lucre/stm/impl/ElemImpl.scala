/*
 *  ElemImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataOutput, DataInput, Serializer}

import scala.annotation.meta.field

object ElemImpl {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = {
    val typeId  = in.readInt()
    val tpe     = getType(typeId)
    tpe.readIdentifiedObj(in, access)
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Elem[S]] = anySer.asInstanceOf[Ser[S]]

  @field private[this] final val sync   = new AnyRef
  @field private[this] final val anySer = new Ser[NoSys]

  // @volatile private var map = Map.empty[Int, Elem.Type]
  @volatile private var map = Map[Int, Elem.Type](evt.Map.typeId -> evt.Map)

  def addType(tpe: Elem.Type): Unit = sync.synchronized {
    val typeId = tpe.typeId
    if (map.contains(typeId))
      throw new IllegalArgumentException(
        s"Element type $typeId (0x${typeId.toHexString}) was already registered ($tpe overrides ${map(typeId)})")

    map += typeId -> tpe
  }

  @inline
  def getType(id: Int): Elem.Type = map.getOrElse(id, sys.error(s"Unknown element type $id (0x${id.toHexString})"))

  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Elem[S]] {
    def write(obj: Elem[S], out: DataOutput): Unit = obj.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = ElemImpl.read(in, access)
  }
}