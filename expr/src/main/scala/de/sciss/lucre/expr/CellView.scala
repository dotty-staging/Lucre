/*
 *  CellView.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.impl.{CellViewImpl => Impl}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{stm, event => evt, expr => _expr}
import de.sciss.serial.Serializer

import scala.language.higherKinds

object CellView {
  def expr[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: Expr[~, A]](x: _Ex[S])(implicit tx: S#Tx,
                                                                    tpe: Type.Expr[A, _Ex]): CellView[S#Tx, A] { type Repr = _Ex[S] } = {
    import tpe.{serializer, varSerializer}
    x match {
      case tpe.Var(vr) =>
        new Impl.ExprVar[S, A, _Ex](tx.newHandle(vr))

      case _ =>
        new Impl.Expr[S, A, _Ex](tx.newHandle(x))
    }
  }

  def exprMap[S <: Sys[S], K, A, _Ex[~ <: Sys[~]] <: _expr.Expr[~, A]](map: evt.Map[S, K, _Ex], key: K)
                                                                     (implicit tx: S#Tx, tpe: Type.Expr[A, _Ex], keyType: evt.Map.Key[K])
  : CellView[S#Tx, Option[A]] { type Repr = Option[_Ex[S]] } = {
    // import tpe.serializer
    map.modifiableOption.fold[CellView[S#Tx, Option[A]] { type Repr = Option[_Ex[S]] }] {
      new Impl.ExprMap[S, K, A, _Ex  /* , Change[A] */](tx.newHandle(map), key /* , ch =>
        if (ch.isSignificant) Some(ch.now) else None */)
    } { mv =>
      new Impl.ExprModMap[S, K, A, _Ex](tx.newHandle(mv), key)
    }
  }

  def attr[S <: Sys[S], A, E[~ <: Sys[~]] <: Expr[~, A]](map: Obj.AttrMap[S], key: String)
                                                        (implicit tx: S#Tx, tpe: Type.Expr[A, E]): CellView.Var[S, Option[A]] { type Repr = Option[E[S]] } = {
    new Impl.AttrImpl[S, A, E](tx.newHandle(map), key)
  }

  def name[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): CellView[S#Tx, String] = {
    implicit val stringEx: Type.Expr[String, StringObj] = StringObj
    attr[S, String, StringObj](obj.attr, Obj.attrName).map(_.getOrElse("<unnamed>"))
  }

  def exprLike[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: Expr[~, A]](x: _Ex[S])
                                                             (implicit tx: S#Tx,
                                                              serializer: Serializer[S#Tx, S#Acc, _Ex[S]]): CellView[S#Tx, A] { type Repr = _Ex[S] } =
    new Impl.Expr[S, A, _Ex](tx.newHandle(x))

  def const[S <: Sys[S], A](value: A): CellView[S#Tx, A] = new Impl.Const(value)

  object Var {
    def unapply[S <: Sys[S], A](view: CellView[S#Tx, A]): Option[Var[S, A]] = view match {
      case vr: Var[S, A] => Some(vr)
      case _ => None
    }
  }
  trait Var[S <: Sys[S], A] extends CellView[S#Tx, A] with stm.Sink[S#Tx, A] {
    def repr_=(value: Repr)(implicit tx: S#Tx): Unit

    def lift(value: A)(implicit tx: S#Tx): Repr

    implicit def serializer: Serializer[S#Tx, S#Acc, Repr]
  }
}
trait CellView[Tx, +A] extends Observable[Tx, A] with stm.Source[Tx, A] {
  def map[B](f: A => B): CellView[Tx, B]

  type Repr

  def repr(implicit tx: Tx): Repr
}