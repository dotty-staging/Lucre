/*
 *  Attr.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.{aux, stm}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.impl.ExpandedAttrUpdate
import de.sciss.lucre.expr.impl.ExAttrBridgeImpl
import de.sciss.lucre.expr.{BooleanObj, CellView, Control, DoubleObj, DoubleVector, Ex, IExpr, IntObj, IntVector, LongObj, SpanLikeObj, SpanObj, StringObj}
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change
import de.sciss.span.{Span, SpanLike}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object Attr {
  trait Like[A] extends ProductWithAux {
    def update(in: Ex[A]): Control
  }

  private[lucre] def resolveNested[S <: Sys[S], A](key: String)(implicit ctx: Ex.Context[S], tx: S#Tx,
                                                   bridge: Bridge[A]): Option[CellView.Var[S, Option[A]]] = {
    @tailrec
    def loop(objOpt: Option[stm.Obj[S]], sub: String): Option[CellView.Var[S, Option[A]]] =
      objOpt match {
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

    loop(ctx.selfOption, key)
  }

  object WithDefault {
    def apply[A](key: String, default: Ex[A])(implicit bridge: Bridge[A]): WithDefault[A] =
      Impl(key, default)

    private final case class Impl[A](key: String, default: Ex[A])(implicit val bridge: Bridge[A])
      extends WithDefault[A] with ProductWithAux {

      override def productPrefix: String = s"Attr$$WithDefault" // serialization

      def update(in: Ex[A]): Control = Attr.Update(in, key)

      def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] = {
        val defaultEx = default.expand[S]
        resolveNested(key).fold(defaultEx) { attrView =>
          import ctx.targets
          new WithDefault.Expanded[S, A](attrView, defaultEx, tx)
        }
      }

      override def aux: scala.List[Aux] = bridge :: Nil
    }

    final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]], default: IExpr[S, A], tx0: S#Tx)
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
  // N.B. we use a trait here not a case class, because
  // we reuse the interface elsewhere (SP -> Artifact)
  trait WithDefault[A] extends Ex[A] with Like[A] {
    def default: Ex[A]
  }

  object Bridge {
    implicit val int      : Bridge[Int        ] = new ExAttrBridgeImpl(IntObj       )
    implicit val long     : Bridge[Long       ] = new ExAttrBridgeImpl(LongObj      )
    implicit val double   : Bridge[Double     ] = new ExAttrBridgeImpl(DoubleObj    )
    implicit val boolean  : Bridge[Boolean    ] = new ExAttrBridgeImpl(BooleanObj   )
    implicit val string   : Bridge[String     ] = new ExAttrBridgeImpl(StringObj    )
    implicit val spanLike : Bridge[SpanLike   ] = new ExAttrBridgeImpl(SpanLikeObj  )
    implicit val span     : Bridge[Span       ] = new ExAttrBridgeImpl(SpanObj      )
    implicit val intVec   : Bridge[Vec[Int   ]] = new ExAttrBridgeImpl(IntVector    )
    implicit val doubleVec: Bridge[Vec[Double]] = new ExAttrBridgeImpl(DoubleVector )

  }
  trait Bridge[A] extends aux.Aux {
    // def peer: Obj.Type

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]]
  }

  final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]], tx0: S#Tx)
                                      (implicit protected val targets: ITargets[S])
    extends IExpr[S, Option[A]] with IGenerator[S, Change[Option[A]]] {

    private[this] val ref = Ref(value(tx0))

    private[this] val obsAttr = attrView.react { implicit tx => now =>
      val before = ref.swap(now)(tx.peer)
      val ch = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Option[A] = attrView()

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[A]]] =
      Some(pull.resolve[Change[Option[A]]])

    def changed: IEvent[S, Change[Option[A]]] = this

    def dispose()(implicit tx: S#Tx): Unit =
      obsAttr.dispose()
  }

  final case class Update[A](source: Ex[A], key: String)(implicit bridge: Attr.Bridge[A])
    extends Control with ProductWithAux {

    override def productPrefix: String = s"Attr$$Update"

    type Repr[S <: Sys[S]] = Disposable[S#Tx]

    protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S] =
      resolveNested(key).fold(Disposable.empty[S#Tx]) { attrView  =>
        new ExpandedAttrUpdate[S, A](source.expand[S], attrView, tx)
      }

    override def aux: scala.List[Aux] = bridge :: Nil
  }
}
final case class Attr[A](key: String)(implicit val bridge: Attr.Bridge[A])
  extends Ex[Option[A]] with Attr.Like[A] with ProductWithAux {

  def update(in: Ex[A]): Control = Attr.Update(in, key)

  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, Option[A]] = {
    ctx.selfOption.fold(Constant(Option.empty[A]).expand[S]) { self =>
      import ctx.targets
      val attrView = bridge.cellView[S](self, key)
      new Attr.Expanded[S, A](attrView, tx)
    }
  }

  override def aux: scala.List[Aux] = bridge :: Nil
}
