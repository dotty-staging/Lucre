/*
 *  Artifact.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import java.net.URI

import de.sciss.lucre.impl.{ArtifactImpl => Impl}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, TFormat}

object Artifact extends Obj.Type with ArtifactPlatform {
  final val typeId = 0x10008

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Artifact[T] =
    format[T].readT(in)

  implicit def format[T <: Txn[T]]: TFormat[T, Artifact[T]] = Impl.format

  def apply[T <: Txn[T]](location: ArtifactLocation[T], child: Child)(implicit tx: T): Artifact.Modifiable[T] =
    Impl(location, child)

  def apply[T <: Txn[T]](location: ArtifactLocation[T], file: Value)(implicit tx: T): Artifact.Modifiable[T] =
    apply(location, Value.relativize(location.directory, file))

  object Value extends ConstFormat[Value] {
    def relativize(parent: Value, sub: Value): Child = {
      val s1 = parent.getScheme
      val s2 = sub   .getScheme
      if (s2 != null && s1 != s2) {
        throw new IllegalStateException(s"Mismatching artifact URI schemes: $s1 / $s2")
      }

      val parentP = parent.normalize().getPath
      val subP    = sub   .normalize().getPath

      if (!subP.startsWith(parentP))
        throw new IllegalArgumentException(s"File $subP is not in a subdirectory of $parentP")

      val res0    = subP.substring(parentP.length)
      val res     = if (res0.isEmpty || res0.charAt(0) != '/') res0 else res0.substring(1)
      Child(res)
    }

    def empty: Value = new URI(null, "", null)

    private final val SER_VERSION = 2

    def read(in: DataInput): Value = {
      val ver = in.readByte()
      if (ver != SER_VERSION) {
        if (ver == 1) { // old school plain path
          val filePath = in.readUTF()
          return fileToURI(filePath)
        }
        sys.error(s"Unexpected serialization version ($ver != $SER_VERSION)")
      }
      val str = in.readUTF()
      if (str.isEmpty) Value.empty else new URI(str)
    }

    def write(v: Value, out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      out.writeUTF(v.toString)
    }
  }
  type Value = URI

  object Modifiable {
    def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Modifiable[T] =
      format[T].readT(in)

    def copy[T <: Txn[T]](from: Artifact[T])(implicit tx: T): Modifiable[T] =
      Impl.copy(from)

    implicit def format[T <: Txn[T]]: TFormat[T, Modifiable[T]] = Impl.modFormat
  }
  trait Modifiable[T <: Txn[T]] extends Artifact[T] {
    def child_=(value: Child)(implicit tx: T): Unit
  }

  final case class Child(path: String)

  // ---- Type.Expr ----

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedArtifact(in)
}
/** An artifact is a file on an external storage. */
trait Artifact[T <: Txn[T]] extends Expr[T, Artifact.Value] {
  import Artifact.{Child, Modifiable}

  def location: ArtifactLocation[T]

  def modifiableOption: Option[Modifiable[T]]

  def child(implicit tx: T): Child
}