/*
 *  CaseDef.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.adjunct.Adjunct.{FromAny, HasDefault}
import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IControl, IExpr, graph}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object CaseDef {
  sealed trait Expanded[S <: Sys[S], A] extends IExpr[S, A] {
    def fromAny: FromAny[A]

    def select(value: Any)(implicit tx: S#Tx): Boolean

    def commit()(implicit tx: S#Tx): Unit
  }
}
sealed trait CaseDef[A] extends Ex[A] with ProductWithAdjuncts {
  type Repr[S <: Sys[S]] <: CaseDef.Expanded[S, A]

  def fromAny: FromAny[A]

  def adjuncts: List[Adjunct] = fromAny :: Nil
}

object Quote {
  private final class ExpandedImpl[S <: Sys[S], A](in: IExpr[S, A])(implicit val fromAny: FromAny[A])
    extends Expanded[S, A] {

    def select(value: Any)(implicit tx: S#Tx): Boolean =
      fromAny.fromAny(value) match {
        case Some(v) if v == in.value  => true
        case _                         => false
      }

    def commit()(implicit tx: S#Tx): Unit = ()

    def value(implicit tx: S#Tx): A = in.value

    def dispose()(implicit tx: S#Tx): Unit = in.dispose()

    def changed: IChangeEvent[S, A] = in.changed
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

//  implicit final class Ops[A](private val x: Var[A]) extends AnyVal {
//    def set(in: Ex[A]): Act = Set(x, in)
//  }

  final case class Set[A](vr: Var[A], in: Ex[A]) extends Act {
    override def productPrefix: String = s"Var$$Set"  // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new SetExpanded(vr.expand[S], in.expand[S])
  }

  final case class Update[A](vr: Var[A], in: Ex[A]) extends Control {
    override def productPrefix: String = s"Var$$Update"  // serialization

    type Repr[S <: Sys[S]] = IControl[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val peer = new UpdateExpanded(vr.expand[S], in.expand[S], tx)
      IControl.wrap(peer)
    }
  }

  trait Expanded[S <: Sys[S], A] extends CaseDef.Expanded[S, A] with IExpr.Var[S, A]

  // ---- private ----

  private final class SetExpanded[S <: Sys[S], A](vr: Var.Expanded[S, A], in: IExpr[S, A])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      // If the 'set' action is interpreted correctly, it should
      // mean that we latch the value; if otherwise we would just
      // use `vr.update(in)`, then we could as well just
      // refer to `in` directly and the operation would not
      // make much sense. And so `Var.Set` works like `ExpandedAttrSet`.
      val v = in.value
      vr.update(new Const.Expanded(v))
    }
  }

  private final class UpdateExpanded[S <: Sys[S], A](vr: Var.Expanded[S, A], source: IExpr[S, A], tx0: S#Tx)
    extends Disposable[S#Tx] {

    private[this] val obs = source.changed.react { implicit tx => upd =>
      vr.update(new Const.Expanded(upd.now))
    } (tx0)

    def dispose()(implicit tx: S#Tx): Unit =
      obs.dispose()
  }

  private final class ExpandedImpl[S <: Sys[S], A](init: IExpr[S, A], tx0: S#Tx)
                                              (implicit protected val targets: ITargets[S], val fromAny: FromAny[A])
    extends Expanded[S, A] with IChangeGenerator[S, A] {

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

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): A =
      if (pull.isOrigin(this)) pull.resolveExpr(this)
      else pull.expr(ref())

    def select(value: Any)(implicit tx: S#Tx): Boolean =
      fromAny.fromAny(value) match {
        case Some(v) =>
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

    def changed: IChangeEvent[S, A] = this
  }

  private final case class Impl[A](init: Ex[A])(implicit val fromAny: FromAny[A]) extends Var[A] {
    override def productPrefix: String = "Var"  // serialization

    def update(in: Ex[A]): Control = Update(this, in)

    def set(in: Ex[A]): Act = Set(this, in)

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new Var.ExpandedImpl[S, A](init.expand[S], tx)
    }
  }
}
trait Var[A] extends Ex[A] with CaseDef[A] with Attr.Like[A] with ProductWithAdjuncts {
  type Repr[S <: Sys[S]] = Var.Expanded[S, A]
}
