/*
 *  Attr.scala
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

import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.impl.{ExpandedAttrSet, ExpandedAttrUpdate}
import de.sciss.lucre.expr.impl.EmptyIAction
import de.sciss.lucre.expr.{CellView, Context, IAction, IControl, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object Attr {
  trait Like[A] extends ProductWithAux {
    def update(in: Ex[A]): Control
    def set   (in: Ex[A]): Act
  }

  private[lucre] def resolveNestedIn[S <: Sys[S], A](objOpt: Option[stm.Obj[S]], key: String)
                                                    (implicit tx: S#Tx,
                                                     bridge: Obj.Bridge[A]): Option[CellView.Var[S, Option[A]]] = {
    @tailrec
    def loop(prev: Option[stm.Obj[S]], sub: String): Option[CellView.Var[S, Option[A]]] =
      prev match {
        case Some(obj) =>
          val i = sub.indexOf(':')
          if (i < 0) {
            val attrView = bridge.cellView[S](obj, sub)
            Some(attrView)
          } else {
            val head = sub.substring(0, i)
            val tail = sub.substring(i + 1)
            val next = obj.attr.get(head)
            loop(next, tail)
          }

        case _ => None
      }

    loop(objOpt, key)
  }

  private[lucre] def resolveNested[S <: Sys[S], A](key: String)(implicit ctx: Context[S], tx: S#Tx,
                                                                bridge: Obj.Bridge[A]): Option[CellView.Var[S, Option[A]]] =
    resolveNestedIn(ctx.selfOption, key)

  object WithDefault {
    def apply[A](key: String, default: Ex[A])(implicit bridge: Obj.Bridge[A]): WithDefault[A] =
      Impl(key, default)

    private final case class Impl[A](key: String, default: Ex[A])(implicit val bridge: Obj.Bridge[A])
      extends WithDefault[A] with ProductWithAux {

      type Repr[S <: Sys[S]] = IExpr[S, A]

      override def productPrefix: String = s"Attr$$WithDefault" // serialization

      def update(in: Ex[A]): Control  = Attr.Update(in, key)
      def set   (in: Ex[A]): Act      = Attr.Set   (in, key)

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        val defaultEx: Repr[S] = default.expand[S]
        resolveNested(key).fold(defaultEx) { attrView =>
          import ctx.targets
          new WithDefault.Expanded[S, A](attrView, defaultEx, tx)
        }
      }

      override def aux: scala.List[Aux] = bridge :: Nil
    }

    private[lucre] final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]], default: IExpr[S, A],
                                                        tx0: S#Tx)
                                                       (implicit protected val targets: ITargets[S])
      extends IExpr[S, A] with IGenerator[S, Change[A]] {

      private[this] val ref = Ref(attrView()(tx0))

      private[this] val obsAttr = attrView.react { implicit tx => now =>
        val before  = ref.swap(now)(tx.peer)
        if (before != now) {
          val before1   = before.getOrElse(default.value)
          val now1      = now   .getOrElse(default.value)
          val ch        = Change(before1, now1)
          if (ch.isSignificant) fire(ch)
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
          Some(pull.resolve)
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
  // N.B. we use a trait here not a case class, because
  // we reuse the interface elsewhere (SP -> Artifact)
  trait WithDefault[A] extends Ex[A] with Like[A] {
    def default: Ex[A]
  }

  private[lucre] final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]], tx0: S#Tx)
                                                     (implicit protected val targets: ITargets[S])
    extends IExpr[S, Option[A]] with IGenerator[S, Change[Option[A]]] {

    // println("Attr.Expanded - created")

    private[this] val ref = Ref(value(tx0))

    private[this] val obsAttr = attrView.react { implicit tx => now =>
      val before = ref.swap(now)(tx.peer)
      val ch = Change(before, now)
      // println(s"Attr.Expanded change $ch")
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Option[A] = attrView()

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[A]]] =
      Some(pull.resolve)

    def changed: IEvent[S, Change[Option[A]]] = this

    def dispose()(implicit tx: S#Tx): Unit = {
      // println("Attr.Expanded - dispose")
      obsAttr.dispose()
    }
  }

  final case class Update[A](source: Ex[A], key: String)(implicit bridge: Obj.Bridge[A])
    extends Control with ProductWithAux {

    override def productPrefix: String = s"Attr$$Update"  // serialization

    type Repr[S <: Sys[S]] = IControl[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val peer = resolveNested(key).fold(Disposable.empty[S#Tx]) { attrView =>
        new ExpandedAttrUpdate[S, A](source.expand[S], attrView, tx)
      }
      IControl.wrap(peer)
    }

    override def aux: scala.List[Aux] = bridge :: Nil
  }

  final case class Set[A](source: Ex[A], key: String)(implicit bridge: Obj.Bridge[A])
    extends Act with ProductWithAux {

    override def productPrefix: String = s"Attr$$Set"  // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      resolveNested(key).fold[IAction[S]](new EmptyIAction) { attrView =>
        new ExpandedAttrSet[S, A](attrView, source.expand[S], tx)
      }

    override def aux: scala.List[Aux] = bridge :: Nil
  }
}
final case class Attr[A](key: String)(implicit val bridge: Obj.Bridge[A])
  extends Ex[Option[A]] with Attr.Like[A] with ProductWithAux {

  type Repr[S <: Sys[S]] = IExpr[S, Option[A]]

  def update(in: Ex[A]): Control  = Attr.Update(in, key)
  def set   (in: Ex[A]): Act      = Attr.Set   (in, key)

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    ctx.selfOption.fold(Const(Option.empty[A]).expand[S]) { self =>
      import ctx.targets
      val attrView = bridge.cellView[S](self, key)
      new Attr.Expanded[S, A](attrView, tx)
    }
  }

  override def aux: scala.List[Aux] = bridge :: Nil
}
