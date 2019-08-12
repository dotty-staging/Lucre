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

import de.sciss.lucre.aux.Aux.FromAny
import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.expr.{CellView, Context, ExprLike, Type}
import de.sciss.lucre.stm.{Disposable, Form, Sys}
import de.sciss.lucre.{expr, stm}
import de.sciss.serial.{DataOutput, Serializer}

import scala.language.higherKinds

abstract class AbstractExObjCanMakeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](peer: Type.Expr[A, _Ex])
  extends Obj.CanMake[A] {

  type Repr[S <: Sys[S]] = _Ex[S]

  def toObj[S <: Sys[S]](value: A)(implicit tx: S#Tx): _Ex[S] =
    peer.newVar(peer.newConst(value))

  def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, _Ex[S]] = peer.serializer
}

final class ExObjCanMakeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](peer: Type.Expr[A, _Ex])
  extends AbstractExObjCanMakeImpl[A, _Ex](peer) {

  def id: Int = Type.ObjBridge.id

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(peer.typeId)
  }
}

abstract class ExObjBridgeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](peer: Type.Expr[A, _Ex])
                                                                      (implicit fa: FromAny[A])
  extends AbstractExObjCanMakeImpl[A, _Ex](peer) with Obj.Bridge[A] {

//  def id: Int = Type.ObjBridge.id

  def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]] =
    CellView.attrUndoOpt[S, A, _Ex](map = obj.attr, key = key)(tx, peer)

  def cellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[A]] = {
    val fallBack = context.selfOption match {
      case Some(obj)  => cellView(obj, key)
      case None       => CellView.const[S, Option[A]](None)
    }
    new CtxView[S](key, context.attr, fallBack)
  }

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(peer.typeId)
  }

  // a hierarchical cell view, first looking at the context attributes, then
  // falling back to another given view (typically from the self's object attributes)
  private final class CtxView[S <: Sys[S]](key: String, attr: Context.Attr[S],
                                           fallBack: CellView[S#Tx, Option[A]])
    extends CellView[S#Tx, Option[A]] {

    def react(fun: S#Tx => Option[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
      val disp1 = fallBack.react { implicit tx => _ =>
        fun(tx)(apply())
      }

      val outer: S#Tx => Option[A] => Unit = { implicit tx => _ =>
        fun(tx)(apply())
      }

      val disp2 = new CellViewImpl.MapLikeExprObs(map = attr, key = key, fun = outer, tx0 = tx) {
        protected def compareTpe(in: Form[S]): Boolean = in match {
          case ex: ExprLike[S, _] =>
            val tr = fa.fromAny(ex.value)
              tr.isSuccess
          case _ => false
        }
      }

      Disposable.seq(disp1, disp2)
    }

    private def outerApply()(implicit tx: S#Tx): Option[A] = {
      val formOpt: Option[Form[S]] = attr.get(key)
      formOpt match {
        case Some(ex: ExprLike[S, _]) =>
          val tr = fa.fromAny(ex.value)
            tr.toOption
        case _ => None
      }
    }

    private def innerApply()(implicit tx: S#Tx): Option[A] =
      fallBack()

    def apply()(implicit tx: S#Tx): Option[A] =
      outerApply() orElse innerApply()
  }
}

final class LegacyObjBridgeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](peer: Type.Expr[A, _Ex])
  extends ExObjBridgeImpl[A, _Ex](peer)(FromAny.empty[A]) with Obj.Bridge[A] {

  def id: Int = Type.ObjBridge.id

  def fromAny(value: Any): Option[A] = None

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(peer.typeId)
  }
}