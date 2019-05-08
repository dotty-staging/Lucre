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
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.{IExpr, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref
import scala.language.higherKinds
import scala.util.Success

object CaseDef {
  sealed trait Expanded[S <: Sys[S], A] extends IExpr[S, A] {
    def select(value: Any)(implicit tx: S#Tx): Boolean

    def commit()(implicit tx: S#Tx): Unit
  }
}
sealed trait CaseDef[A] extends Ex[A] with ProductWithAux {
  type Repr[S <: Sys[S]] <: CaseDef.Expanded[S, A]

  def fromAny: FromAny[A]

  def aux: List[Aux] = fromAny :: Nil
}

object Quote {
  private final class ExpandedImpl[S <: Sys[S], A](in: IExpr[S, A])(implicit fromAny: FromAny[A])
    extends Expanded[S, A] {

    def select(value: Any)(implicit tx: S#Tx): Boolean =
      fromAny.fromAny(value) match {
        case Success(v) if v == in.value  => true
        case _                            => false
      }

    def commit()(implicit tx: S#Tx): Unit = ()

    def value(implicit tx: S#Tx): A = in.value

    def dispose()(implicit tx: S#Tx): Unit = in.dispose()

    def changed: IEvent[S, Change[A]] = in.changed
  }

  trait Expanded[S <: Sys[S], A] extends CaseDef.Expanded[S, A]
}
final case class Quote[A](in: Ex[A])(implicit val fromAny: FromAny[A])
  extends CaseDef[A] {

  type Repr[S <: Sys[S]] = CaseDef.Expanded[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
    new Quote.ExpandedImpl(in.expand[S])
}

object Var {
  def apply[A](init: Ex[A])(implicit from: FromAny[A]): Var[A] = Impl(init)

  def apply[A]()(implicit from: FromAny[A], default: HasDefault[A]): Var[A] =
    Impl(Const(default.defaultValue))

  trait Expanded[S <: Sys[S], A] extends CaseDef.Expanded[S, A] with IExpr.Var[S, A]

  private final class ExpandedImpl[S <: Sys[S], A](init: IExpr[S, A], tx0: S#Tx)
                                              (implicit protected val targets: ITargets[S], val fromAny: FromAny[A])
    extends Expanded[S, A] with IGenerator[S, Change[A]] {

    private[this] val ref     = Ref(init)
    private[this] val selRef  = Ref.make[A]

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

    def select(value: Any)(implicit tx: S#Tx): Boolean =
      fromAny.fromAny(value) match {
        case Success(v) =>
          selRef() = v
          true
        case _ =>
          false
      }

//    def commit()(implicit tx: S#Tx): Unit =
//      ref() = new graph.Const.Expanded(selRef())

    def commit()(implicit tx: S#Tx): Unit =
      update(new graph.Const.Expanded(selRef()))

    def dispose()(implicit tx: S#Tx): Unit =
      ref().changed -/-> changed

    def changed: IEvent[S, Change[A]] = this
  }

  private final case class Impl[A](init: Ex[A])(implicit val fromAny: FromAny[A]) extends Var[A] {
    override def productPrefix: String = "Var"  // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new Var.ExpandedImpl[S, A](init.expand[S], tx)
    }
  }
}
trait Var[A] extends Ex[A] with CaseDef[A] with ProductWithAux {
  type Repr[S <: Sys[S]] = Var.Expanded[S, A]
}
