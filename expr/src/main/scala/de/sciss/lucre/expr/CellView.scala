/*
 *  CellView.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.impl.CellViewImpl.{FlatMapImpl, MapImpl, OptionOrElseImpl}
import de.sciss.lucre.expr.impl.{CellViewImpl => Impl}
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.{stm, event => evt, expr => _expr}
import de.sciss.serial.Serializer

import scala.language.higherKinds

object CellView {
  def empty[Tx, A]: CellView[Tx, Option[A]] = Empty.asInstanceOf[CellView[Tx, Option[A]]]

  def expr[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: Expr[~, A]](x: _Ex[S])(implicit tx: S#Tx,
                                                                    tpe: Type.Expr[A, _Ex]): CellView[S#Tx, A] =
    Impl.expr[S, A, _Ex](x)

  def exprMap[S <: Sys[S], K, A, _Ex[~ <: Sys[~]] <: _expr.Expr[~, A]](map: evt.Map[S, K, _Ex], key: K)
                                                                     (implicit tx: S#Tx, tpe: Type.Expr[A, _Ex],
                                                                      keyType: evt.Map.Key[K]): CellView[S#Tx, Option[A]] =
    Impl.exprMap[S, K, A, _Ex](map, key)

  def attr[S <: Sys[S], A, E[~ <: Sys[~]] <: Expr[~, A]](map: Obj.AttrMap[S], key: String)
                                                        (implicit tx: S#Tx, tpe: Type.Expr[A, E]): CellView.Var[S#Tx, Option[A]] =
    Impl.attr[S, A, E](map, key)

  /** Additionally uses undo manager when present. */
  def attrUndoOpt[S <: Sys[S], A, E[~ <: Sys[~]] <: Expr[~, A]](map: Obj.AttrMap[S], key: String)
                                                               (implicit tx: S#Tx, tpe: Type.Expr[A, E]): CellView.Var[S#Tx, Option[A]] =
    Impl.attrUndoOpt[S, A, E](map, key)

  def name[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): CellView[S#Tx, String] = {
    implicit val stringEx: Type.Expr[String, StringObj] = StringObj
    attr[S, String, StringObj](obj.attr, Obj.attrName).map(_.getOrElse("<unnamed>"))
  }

  def exprLike[S <: Sys[S], A, _Ex[~ <: Sys[~]] <: Expr[~, A]](x: _Ex[S])
                                                             (implicit tx: S#Tx,
                                                              serializer: Serializer[S#Tx, S#Acc, _Ex[S]]): CellView[S#Tx, A] =
    Impl.exprLike[S, A, _Ex](x)

  def const[S <: Sys[S], A](value: A): CellView[S#Tx, A] =
    Impl.const[S, A](value)

  object Var {
    def unapply[Tx, A](view: CellView[Tx, A]): Option[Var[Tx, A]] = view match {
      case vr: Var[Tx, A] => Some(vr)
      case _              => None
    }

    def empty[Tx, A]: Var[Tx, Option[A]] = Empty.asInstanceOf[Var[Tx, Option[A]]]

    private object Empty extends Var[Any, Option[Nothing]] {
      def react(fun: Any => Option[Nothing] => Unit)(implicit tx: Any): Disposable[Any] =
        Disposable.empty

      def apply()(implicit tx: Any): Option[Nothing] = None

      def update(v: Option[Nothing])(implicit tx: Any): Unit = ()
    }
  }
  trait Var[Tx, A] extends CellView[Tx, A] with stm.Sink[Tx, A]

  object VarR {
    def unapply[S <: Sys[S], A](view: CellView[S#Tx, A]): Option[VarR[S, A]] = view match {
      case vr: VarR[S, A] => Some(vr)
      case _              => None
    }
  }
  /** A mutable cell view representing serializable elements. */
  trait VarR[S <: Sys[S], A] extends Var[S#Tx, A] {
    type Repr

    def repr(implicit tx: S#Tx): Repr

    def repr_=(value: Repr)(implicit tx: S#Tx): Unit

    def lift(value: A)(implicit tx: S#Tx): Repr

    implicit def serializer: Serializer[S#Tx, S#Acc, Repr]
  }

  implicit final class Ops[Tx, A](private val in: CellView[Tx, A]) extends AnyVal {
    def map[B](f: A => B): CellView[Tx, B] =
      new MapImpl(in, f)
  }

  implicit final class OptionOps[Tx, A](private val in: CellView[Tx, Option[A]]) extends AnyVal {
    def orElse[B >: A](that: CellView[Tx, Option[B]]): CellView[Tx, Option[B]] =
      new OptionOrElseImpl(in, that)

    def flatMapTx[B](f: Tx => A => Option[B]): CellView[Tx, Option[B]] =
      new FlatMapImpl(in, f)
  }

  // ---- impl ----
  private object Empty extends CellView[Any, Option[Nothing]] {
    def react(fun: Any => Option[Nothing] => Unit)(implicit tx: Any): Disposable[Any] =
      Disposable.empty

    def apply()(implicit tx: Any): Option[Nothing] = None
  }
}

/** A `CellView` is an in-memory view of a transactional object that fires updates when the object changes.
  *
  * It is important that the cell-view is ''not'' a `Disposable`, which means we do not have to track its
  * life cycle. A `Disposable` of course is generated from the `react` call.
  */
trait CellView[Tx, +A] extends Observable[Tx, A] with stm.Source[Tx, A]