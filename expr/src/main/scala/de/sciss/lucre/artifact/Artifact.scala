/*
 *  Artifact.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.artifact

import java.io.File

import de.sciss.lucre.artifact.impl.{ArtifactImpl => Impl}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{DataInput, Serializer}

import scala.annotation.tailrec

object Artifact extends Obj.Type {
  final val typeId = 0x10008

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] =
    serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Artifact[S]] = Impl.serializer

  def apply[S <: Sys[S]](location: ArtifactLocation[S], child: Child)(implicit tx: S#Tx): Artifact.Modifiable[S] =
    Impl(location, child)

  def apply[S <: Sys[S]](location: ArtifactLocation[S], file: File)(implicit tx: S#Tx): Artifact.Modifiable[S] =
    apply(location, relativize(location.directory, file))

  def relativize(parent: File, sub: File): Child = {
    // Note: .getCanonicalFile will resolve symbolic links.
    // In order to support artifacts being symbolic links
    // inside a parent folder, we must not resolve them!

    val can     = sub   .getAbsoluteFile // .getCanonicalFile
    val base    = parent.getAbsoluteFile // .getCanonicalFile

    @tailrec def loop(res: File, left: File): File = {
      if (left == null)
        throw new IllegalArgumentException(s"File $sub is not in a subdirectory of $parent")

      if (left == base) res
      else {
        val last  = left.getName
        val init  = left.getParentFile
        loop(new File(last, res.getPath), init)
      }
    }

    val cf = loop(new File(can.getName), can.getParentFile)
    Child(cf.getPath)
  }

  type Value = File

  object Modifiable {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      serializer[S].read(in, access)

    def copy[S <: Sys[S]](from: Artifact[S])(implicit tx: S#Tx): Modifiable[S] =
      Impl.copy(from)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] = Impl.modSerializer
  }
  trait Modifiable[S <: Sys[S]] extends Artifact[S] {
    def child_=(value: Child)(implicit tx: S#Tx): Unit
  }

  final case class Child(path: String)

  // ---- Type.Expr ----

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedArtifact(in, access)
}
/** An artifact is a file on an external storage. */
trait Artifact[S <: Sys[S]] extends Expr[S, Artifact.Value] {
  import Artifact.{Child, Modifiable}

  def location: ArtifactLocation[S]
  def modifiableOption: Option[Modifiable[S]]
  def child(implicit tx: S#Tx): Child
}