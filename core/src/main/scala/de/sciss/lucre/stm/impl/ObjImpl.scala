/*
 *  ObjImpl.scala
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
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.meta.field

object ObjImpl {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val typeId  = in.readInt()
    val tpe     = getType(typeId)
    tpe.readIdentifiedObj(in, access)
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Obj[S]] = anySer.asInstanceOf[Ser[S]]

  @field private[this] final val sync   = new AnyRef
  @field private[this] final val anySer = new Ser[NoSys]

  @volatile private var map = Map[Int, Obj.Type](evt.Map.typeId -> evt.Map)

  def addType(tpe: Obj.Type): Unit = sync.synchronized {
    val typeId = tpe.typeId
    if (map.contains(typeId))
      throw new IllegalArgumentException(s"Object type $typeId was already registered ($tpe overrides ${map(typeId)})")

    map += typeId -> tpe
  }

  @inline
  def getType(id: Int): Obj.Type = map.getOrElse(id, sys.error(s"Unknown object type $id"))

  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Obj[S]] {
    def write(obj: Obj[S], out: DataOutput): Unit = obj.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = ObjImpl.read(in, access)
  }

  //  implicit def attrSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, AttrMap[S]] = anyAttrSer.asInstanceOf[AttrSer[S]]
  //
  //  @field private[this] final val anyAttrSer = new AttrSer[NoSys]
  //
  //  private final class AttrSer[S <: Sys[S]] extends ObjSerializer[S, AttrMap[S]] {
  //    protected def tpe: Obj.Type = ...
  //  }
  //
  //  private final class AttrImpl[S <: Sys[S]](val targets: Targets[S])
  //    extends SingleNode[S, AttrUpdate[S]] with AttrMap[S] {
  //
  //
  //    protected def writeData(out: DataOutput): Unit = ...
  //
  //    def changed: EventLike[S, AttrUpdate[S]] = ...
  //
  //    def tpe: Type = ...
  //
  //    protected def disposeData()(implicit tx: S#Tx): Unit = ...
  //  }
  //
  //  trait AttrMap[S <: Sys[S]] extends Node[S] with Publisher[S, AttrUpdate[S]] {
  //    def put   (key: String, value: Obj[S])(implicit tx: S#Tx): Unit
  //    def get   (key: String)(implicit tx: S#Tx): Option[Obj[S]]
  //    def remove(key: String)(implicit tx: S#Tx): Unit
  //
  //    def iterator(implicit tx: S#Tx): Iterator[(String, Obj[S])]
  //  }
}
