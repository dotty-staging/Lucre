/*
 *  ExAttrWithDefault.scala
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
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.expr
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object ExAttrWithDefault {
  private final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]], default: IExpr[S, A], tx0: S#Tx)
    extends IExpr[S, A] with IGenerator[S, Change[A]] {

    protected val targets: ITargets[S] = ITargets[S]

    private[this] val ref = Ref(attrView()(tx0))

    private[this] val obsAttr = attrView.react { implicit tx => now =>
      val before  = ref.swap(now)(tx.peer)
      if (before != now) {
        val before1   = before.getOrElse(default.value)
        val now1      = now   .getOrElse(default.value)
        val ch        = Change(before1, now1)
        fire(ch)
      }
    } (tx0)

    default.changed.--->(this)(tx0)

    def value(implicit tx: S#Tx): A = {
      val opt = attrView()
      opt.getOrElse(default.value)
    }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
      val dch = default.changed
      if (pull.contains(dch) && ref.get(tx.peer).isEmpty) {
        pull(dch)
      } else if (pull.isOrigin(this)) {
        Some(pull.resolve[Change[A]])
      } else {
        None
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      default.changed -/-> this
      obsAttr.dispose()
    }

    def changed: IEvent[S, Change[A]] = this
  }
}
final case class ExAttrWithDefault[A](key: String, default: Ex[A])(implicit tpe: Type.Aux[A]) extends Ex[A] {
  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] = {
    ctx.selfOption.fold(default.expand[S]) { self =>
      val attrView = CellView.attr[S, A, tpe.E](self.attr, key)(tx, tpe.peer)
      new expr.ExAttrWithDefault.Expanded[S, A](attrView, default.expand[S], tx)
    }
  }

  def aux: scala.List[Aux] = tpe :: Nil
}
