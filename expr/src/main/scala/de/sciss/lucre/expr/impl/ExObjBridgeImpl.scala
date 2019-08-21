/*
 *  ExObjBridgeImpl.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.expr.graph.impl.AbstractCtxCellView
import de.sciss.lucre.expr.{CellView, Context, Type}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{expr, stm}
import de.sciss.serial.{DataOutput, Serializer}

import scala.language.higherKinds

abstract class AbstractExObjCanMakeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](tpe: Type.Expr[A, _Ex])
  extends Obj.CanMake[A] {

  type Repr[S <: Sys[S]] = _Ex[S]

  def toObj[S <: Sys[S]](value: A)(implicit tx: S#Tx): _Ex[S] =
    tpe.newVar(tpe.newConst(value))

  def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, _Ex[S]] = tpe.serializer
}

final class ExObjBridgeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](tpe: Type.Expr[A, _Ex])
  extends AbstractExObjCanMakeImpl[A, _Ex](tpe) with Obj.Bridge[A] {

  def id: Int = Type.ObjBridge.id

  def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]] =
    CellView.attrUndoOpt[S, A, _Ex](map = obj.attr, key = key)(tx, tpe)

  def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[A]] =
    new AbstractCtxCellView[S, A](context.attr, key) {
      protected def tryParse(value: Any)(implicit tx: S#Tx): Option[A] =
        tpe.tryParse(value)
    }

  def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[A] = {
    val opt = obj.attr.get(key)
    opt match {
      case Some(v) if v.tpe.typeId == tpe.typeId =>
        val vt = v.asInstanceOf[_Ex[S]]
        Some(vt.value)

      case _ => None
    }
  }

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(tpe.typeId)
  }
}