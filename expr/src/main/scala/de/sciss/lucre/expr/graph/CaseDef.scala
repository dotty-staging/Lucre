/*
 *  CaseDef.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.aux.Aux.{FromAny, HasDefault}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.expr.{Ex, IExpr, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref

sealed trait CaseDef[A] extends Ex[A] with ProductWithAux {
  def fromAny: FromAny[A]

  def aux: List[Aux] = fromAny :: Nil
}

final case class Quote[A](in: Ex[A])(implicit val fromAny: FromAny[A])
  extends CaseDef[A] {

  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A] = in.expand[S]
}

object Var {
  def apply[A](init: Ex[A])(implicit from: FromAny[A]): Var[A] = Impl(init)

  def apply[A]()(implicit from: FromAny[A], default: HasDefault[A]): Var[A] =
    Impl(Const(default.defaultValue))

  private final class Expanded[S <: Sys[S], A](init: IExpr[S, A], tx0: S#Tx)
                                              (implicit protected val targets: ITargets[S])
    extends IExpr.Var[S, A] with IGenerator[S, Change[A]]{

    private[this] val ref = Ref(init)

    init.changed.--->(changed)(tx0)

    def apply()(implicit tx: S#Tx): IExpr[S, A] = ref()

    def value(implicit tx: S#Tx): A = ref().value

    def update(v: IExpr[S, A])(implicit tx: S#Tx): Unit = {
      val before = ref()
      if (before != v) {
        before.changed -/-> this.changed
        ref() = v
        v     .changed ---> this.changed

        val beforeV = before.value
        val exprV   = v     .value
        fire(Change(beforeV, exprV))
      }
    }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
      if (pull.isOrigin(this)) {
        Some(pull.resolve[Change[A]])
      } else {
        pull(ref().changed)
      }

    def dispose()(implicit tx: S#Tx): Unit =
      ref().changed -/-> changed

    def changed: IEvent[S, Change[A]] = this
  }

  private final case class Impl[A](init: Ex[A])(implicit val fromAny: FromAny[A]) extends Var[A] {
    override def productPrefix: String = "Var"  // serialization

    override def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr.Var[S, A] = {
      import ctx.targets
      new graph.Var.Expanded[S, A](init.expand[S], tx)
    }
  }
}
trait Var[A] extends Ex[A] with CaseDef[A] with ProductWithAux {
  override def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr.Var[S, A]
}
