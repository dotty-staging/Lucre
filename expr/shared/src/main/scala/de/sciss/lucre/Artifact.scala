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
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, TFormat}

object Artifact extends Obj.Type /*with ArtifactPlatform*/ {
  final val typeId = 0x10008

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Artifact[T] =
    format[T].readT(in)

  implicit def format[T <: Txn[T]]: TFormat[T, Artifact[T]] = Impl.format

  def apply[T <: Txn[T]](location: ArtifactLocation[T], child: Child)(implicit tx: T): Artifact.Modifiable[T] =
    Impl(location, child)

  def apply[T <: Txn[T]](location: ArtifactLocation[T], file: Value)(implicit tx: T): Artifact.Modifiable[T] =
    apply(location, Value.relativize(location.directory, file))

  object Value extends ConstFormat[Value] {
    /** This can be imported to be able to write `value.name` or `value.replaceExt("foo")` */
    implicit final class Ops(private val v: Value) extends AnyVal {
      def path: String = Value.path(v)
      def name: String = Value.name(v)
      def base: String = Value.base(v)
      def extL: String = Value.extL(v)

      def parentOption: Option[Value] = Value.parentOption(v)

      def replaceExt (ext : String): Value = Value.replaceExt (v, ext )
      def replaceName(name: String): Value = Value.replaceName(v, name)

      def / (sub: String): Value = Value.append(v, sub)
    }

    def path(v: Value): String = v.getPath

    def name(v: Value): String = {
      val p = v.normalize().getPath
      val i = p.lastIndexOf('/') + 1
      p.substring(i)
    }

    def base(v: Value): String = {
      val p = v.normalize().getPath
      val i = p.lastIndexOf('/') + 1
      val n = p.substring(i)
      val j = n.lastIndexOf('.')
      if (j < 0) n else n.substring(0, j)
    }

    def extL(v: Value): String = {
      val p   = v.normalize().getPath
      val i   = p.lastIndexOf('/') + 1
      val n   = p.substring(i)
      val j   = n.lastIndexOf('.')
      val ext = if (j < 0) "" else n.substring(j + 1)
      // Locale.US -- not available on Scala.js ; rely on user setting JVM's locale appropriately...
      ext.toLowerCase()
    }

    def replaceExt(v: Value, ext: String): Value = {
      val p   = v.normalize().getPath
      val i     = p.lastIndexOf('/') + 1
      val n     = p.substring(i)
      val j     = n.lastIndexOf('.')
      val base  = if (j < 0) n else n.substring(0, j)
      val extP  = if (ext.startsWith(".")) ext else "." + ext
      val pNew  = base + extP
      val scheme  = v.getScheme
      new URI(scheme, pNew, null)
    }

    def replaceName(v: Value, name: String): Value = {
      val p       = v.normalize().getPath
      val i       = p.lastIndexOf('/') + 1
      val pNew    = p.substring(0, i) + name
      val scheme  = v.getScheme
      new URI(scheme, pNew, null)
    }

    def parentOption(v: Value): Option[Value] = {
      val p = v.normalize().getPath
      val j = if (p.endsWith("/")) p.length - 2 else p.length - 1
      val i = p.lastIndexOf('/', j)
      if (i < 0) None else {
        val pp      = if (i == 0) "/" else p.substring(0, i)
        val scheme  = v.getScheme
        Some(new URI(scheme, pp, null))
      }
    }

    def append(parent: Value, sub: String): Value = {
      val parent0 = parent.normalize().getPath
      val parentS = if (parent0.isEmpty || parent0.endsWith("/")) parent0 else s"$parent0/"
      val path    = s"$parentS$sub"
      new URI("idb", path, null)
    }

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

    private final val SER_VERSION = 1

    def read(in: DataInput): Value = {
      val ver = in.readByte()
      if (ver != SER_VERSION) sys.error(s"Unexpected serialization version ($ver != ${SER_VERSION})")
      val s = in.readUTF()
      new URI(s)
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