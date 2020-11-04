/*
 *  Artifact.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.serial.{DataInput, TFormat}

object Artifact extends Obj.Type with ArtifactPlatform {
  final val typeId = 0x10008

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Artifact[T] =
    format[T].readT(in)

  implicit def format[T <: Txn[T]]: TFormat[T, Artifact[T]] = Impl.format

  def apply[T <: Txn[T]](location: ArtifactLocation[T], child: Child)(implicit tx: T): Artifact.Modifiable[T] =
    Impl(location, child)

  def apply[T <: Txn[T]](location: ArtifactLocation[T], file: Value)(implicit tx: T): Artifact.Modifiable[T] =
    apply(location, relativize(location.directory, file))

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