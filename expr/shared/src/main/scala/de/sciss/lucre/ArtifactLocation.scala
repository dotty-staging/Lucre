/*
 *  ArtifactLocation.scala
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

import de.sciss.lucre
import de.sciss.lucre.Artifact.Value
import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.ExprTypeImpl
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

object ArtifactLocation extends ExprTypeImpl[Value, ArtifactLocation] {
  import lucre.{ArtifactLocation => Repr}

  final val typeId = 0x10003

//  def tmp[T <: Txn/*[T]*/]()(implicit tx: T): Const[T] = {
//    val dir   = File.createTemp("artifacts", "tmp", directory = true)
//    newConst(dir)
//  }

//  implicit def valueFormat: ConstFormat[Value] = TFormat.File

  implicit final object valueFormat extends ConstFormat[Value] {
    def write(v: Value, out: DataOutput): Unit = {
      val str = v.toString
      out.writeUTF(str)
    }

    def read(in: DataInput): Value = {
      val str = in.readUTF()
      new URI(str)
    }
  }

  def tryParse(value: Any): Option[Value] = value match {
    case loc: Value => Some(loc)
    case _          => None
  }

  protected def mkConst[T <: Txn/*[T]*/](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  protected def mkVar[T <: Txn/*[T]*/](targets: Targets[T], vr: lucre.Var[T, E[T]], connect: Boolean)
                                  (implicit tx: T): Var[T] = {
    val res = new _Var[T](tx, targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn/*[T]*/](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn/*[T]*/](val tx: T, val targets: Targets[T], val ref: lucre.Var[T, E[T]])
    extends VarImpl[T] with Repr[T]
}
/** An artifact location is a directory on an external storage. */
trait ArtifactLocation[T <: Txn/*[T]*/] extends Expr[T, Value] {
  /** Alias for `value` */
  def directory(implicit tx: T): Value = value
}