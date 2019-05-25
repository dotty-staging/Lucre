/*
 *  Folder.scala
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

import de.sciss.lucre.event.EventLike
import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.{FolderImpl => Impl}
import de.sciss.serial.{DataInput, Serializer}

object Folder extends Obj.Type {
  final val typeId = 0x10000

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Folder[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Folder[S] =
    serializer[S].read(in, access)

  type Update [S <: Sys[S]]           = stm.List.Update [S, Obj[S], Folder[S]]
  type Change [S <: Sys[S]]           = stm.List.Change [S, Obj[S]]
  type Added  [S <: Sys[S]]           = stm.List.Added  [S, Obj[S]]
  val Added  : stm.List.Added   .type = stm.List.Added
  type Removed[S <: Sys[S]]           = stm.List.Removed[S, Obj[S]]
  val Removed: stm.List.Removed .type = stm.List.Removed

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Folder[S]] =
    Impl.serializer[S]
}
trait Folder[S <: Sys[S]] extends stm.List.Modifiable[S, Obj[S]] {
  /** This is simply because we inherit from `stm.List`. We refine the return type here. */
  override def modifiableOption: Option[Folder[S]]

  override def changed: EventLike[S, List.Update[S, Obj[S], Folder[S]]]
}