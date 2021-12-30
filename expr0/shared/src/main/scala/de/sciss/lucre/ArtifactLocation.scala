/*
 *  ArtifactLocation.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import java.net.URI
import de.sciss.lucre
import de.sciss.lucre.Artifact.Value
import de.sciss.lucre.{Var => LVar}
import de.sciss.lucre.Event.Targets
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.impl.ExprTypeImpl
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object ArtifactLocation extends ExprTypeImpl[Value, ArtifactLocation] {
  import lucre.{ArtifactLocation => Repr}

  final val typeId = 0x10003

//  def tmp[T <: Txn[T]]()(implicit tx: T): Const[T] = {
//    val dir   = File.createTemp("artifacts", "tmp", directory = true)
//    newConst(dir)
//  }

//  implicit def valueFormat: ConstFormat[Value] = TFormat.File

  final val valueName = "URI"

  implicit object valueFormat extends ConstFormat[Value] {
    private final val SER_VERSION = 2

    def write(v: Value, out: DataOutput): Unit = {
      val str = v.toString
      out.writeByte(SER_VERSION )
      out.writeUTF(str)
    }

    def read(in: DataInput): Value = {
      // This is quite tricky because we
      // did not use cookies before, and we
      // need to be able to read old
      // artifact values that were plain
      // File paths.
      // Since writeUTF writes a big-endian
      // short of the size of the byte-encoded
      // string, putting a single version byte
      // that is at least 2 is "OK", as it will
      // just produce problems with paths longer
      // than 511 bytes, which we can exclude historically.
      val p   = in.position
      val rem = in.size - p
      if (rem >= 1) {
        val ver = in.readByte()
        if (ver == SER_VERSION) {
          val str = in.readUTF()
          if (str.isEmpty) Value.empty else new URI(str)
        } else {
          if (ver > SER_VERSION) sys.error(s"Unexpected serialization version ($ver != $SER_VERSION)")
          in.position = p
          val filePath = in.readUTF()
          Artifact.fileToURI(filePath)
        }
      } else {
        Value.empty
      }
    }
  }

  def tryParse(value: Any): Option[Value] = value match {
    case loc: Value => Some(loc)
    case _          => None
  }

  override protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  override protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                           (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  override protected def mkProgram[T <: Txn[T]](targets : Targets[T],
                                                program : LVar[T, Ex[A]],
                                                sources : LVar[T, Vec[Event[T, Any]]],
                                                value   : LVar[T, A],
                                                connect : Boolean)
                                               (implicit tx: T): Program[T] = {
    val res = new _Program[T](targets, programRef = program, sourcesRef = sources, valueRef = value)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  private[this] final class _Program[T <: Txn[T]](val targets   : Targets[T],
                                                  val programRef: LVar[T, Ex[A]],
                                                  val sourcesRef: LVar[T, Vec[Event[T, Any]]],
                                                  val valueRef  : LVar[T, A]
                                                 )
    extends ProgramImpl[T] with Repr[T]
}
/** An artifact location is a directory on an external storage. */
trait ArtifactLocation[T <: Txn[T]] extends Expr[T, Value] {
  /** Alias for `value` */
  def directory(implicit tx: T): Value = value
}