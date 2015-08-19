package de.sciss.lucre.stm
package impl

import de.sciss.serial.{DataOutput, DataInput, Serializer}

import scala.annotation.meta.field

object ObjImpl {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val typeID  = in.readInt()
    val tpe     = map.getOrElse(typeID, sys.error(s"Unknown object type $typeID"))
    tpe.readIdentifiedObj(in, access)
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Obj[S]] = anySer.asInstanceOf[Ser[S]]

  @field private[this] final val sync   = new AnyRef
  @field private[this] final val anySer = new Ser[NoSys]

  @volatile private var map = Map.empty[Int, Obj.Type]

  def register(tpe: Obj.Type): Unit = sync.synchronized {
    val typeID = tpe.typeID
    if (map.contains(typeID))
      throw new IllegalArgumentException(s"Object type $typeID was already registered")

    map += typeID -> tpe
  }

  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Obj[S]] {
    def write(obj: Obj[S], out: DataOutput): Unit = obj.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = ObjImpl.read(in, access)
  }
}
