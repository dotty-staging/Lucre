/*
 *  ExObjBridgeImpl.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.expr.graph.impl.AbstractCtxCellView
import de.sciss.lucre.expr.{CellView, Context, Type}
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.lucre.{expr, stm}
import de.sciss.serial.{DataOutput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds

abstract class AbstractExObjBridgeImpl[A, B <: A, _Ex[~ <: Sys[~]] <: expr.Expr[~, B]](tpe: Type.Expr[B, _Ex])
  extends Obj.CanMake[A] with Obj.Bridge[A] {

  type Repr[S <: Sys[S]] = _Ex[S]

  protected def encode(in: A): B

  final def toObj[S <: Sys[S]](value: A)(implicit tx: S#Tx): _Ex[S] =
    tpe.newVar(tpe.newConst(encode(value)))

  final def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, _Ex[S]] = tpe.serializer

  final def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[A]] =
    new AbstractCtxCellView[S, A](context.attr, key) {
      protected def tryParseValue(value: Any)(implicit tx: S#Tx): Option[A] =
        tpe.tryParse(value)

      protected def tryParseObj(obj: stm.Obj[S])(implicit tx: S#Tx): Option[A] =
        if (obj.tpe === tpe) Some(obj.asInstanceOf[_Ex[S]].value) else None
    }

  final def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[A] = {
    val opt = obj.attr.get(key)
    opt match {
      case Some(v) if v.tpe.typeId === tpe.typeId =>
        val vt = v.asInstanceOf[_Ex[S]]
        Some(vt.value)

      case _ => None
    }
  }

  final def tryParseObj[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[A] =
    if (obj.tpe.typeId === tpe.typeId) {
      val vt = obj.asInstanceOf[_Ex[S]]
      Some(vt.value)
    } else {
      None
    }

  override final def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(tpe.typeId)
  }
}

final class ExObjBridgeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](tpe: Type.Expr[A, _Ex])
  extends AbstractExObjBridgeImpl[A, A, _Ex](tpe) {

  def id: Int = Type.ObjBridge.id

  protected def encode(in: A): A = in

  def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[A]] =
    CellView.attrUndoOpt[S, A, _Ex](map = obj.attr, key = key)(tx, tpe)
}

final class ExSeqObjBridgeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, Vec[A]]](tpe: Type.Expr[Vec[A], _Ex])
  extends AbstractExObjBridgeImpl[Seq[A], Vec[A], _Ex](tpe) {

  def id: Int = Type.SeqObjBridge.id

  protected def encode(in: Seq[A]): Vec[A] = in.toIndexedSeq

  def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[Seq[A]]] = {
    val peer = CellView.attrUndoOpt[S, Vec[A], _Ex](map = obj.attr, key = key)(tx, tpe)
    new CellView.Var[S#Tx, Option[Seq[A]]] {
      def update(v: Option[Seq[A]])(implicit tx: S#Tx): Unit =
        peer.update(v.map(_.toIndexedSeq))

      def apply()(implicit tx: S#Tx): Option[Seq[A]] = peer()

      def react(fun: S#Tx => Option[Seq[A]] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
        peer.react(fun)
    }
  }
}