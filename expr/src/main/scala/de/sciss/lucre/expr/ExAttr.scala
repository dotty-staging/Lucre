/*
 *  ExAttr.scala
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

import de.sciss.lucre.aux.Aux
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.graph.Constant
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

object ExAttr {
  private final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]])
    extends IExpr[S, Option[A]] with IEventImpl[S, Change[Option[A]]] {

    protected val targets: ITargets[S] = ITargets[S]

    def value(implicit tx: S#Tx): Option[A] = attrView()

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[A]]] =
      Some(pull.resolve[Change[Option[A]]])

    def changed: IEvent[S, Change[Option[A]]] = this

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
final case class ExAttr[A](key: String)(implicit tpe: Type.Aux[A]) extends Ex[Option[A]] {
  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, Option[A]] = {
    ctx.selfOption.fold(Constant(Option.empty[A]).expand[S]) { self =>
      val attrView = CellView.attr[S, A, tpe.E](self.attr, key)(tx, tpe.peer)
      new expr.ExAttr.Expanded[S, A](attrView)
    }
  }

  def aux: scala.List[Aux] = tpe :: Nil
}
