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

import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.{aux, expr}
import de.sciss.lucre.expr.graph.Constant
import de.sciss.lucre.expr.impl.ExAttrBridgeImpl
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.model.Change
import de.sciss.span.{Span, SpanLike}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object ExAttr {
  trait Like[A] extends ProductWithAux {
    def key: String

    def bridge: Bridge[A]

    //  def <--- (that: Ex.Var[A]): Unit = that ---> this
    //  def ---> (that: Ex.Var[A]): Unit = that <--- this
    //  def <--> (that: Ex.Var[A]): Unit = that <--> this
  }

  object WithDefault {
    private final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]], default: IExpr[S, A], tx0: S#Tx)
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
  final case class WithDefault[A](key: String, default: Ex[A])(implicit val bridge: Bridge[A])
    extends Ex[A] with Like[A] {

    override def productPrefix: String = s"ExAttr$$WithDefault" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] = {
      ctx.selfOption.fold(default.expand[S]) { self =>
        import ctx.targets
        val attrView = bridge.cellView[S](self, key)
        new WithDefault.Expanded[S, A](attrView, default.expand[S], tx)
      }
    }

    def aux: scala.List[Aux] = bridge :: Nil
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

    def cellView[S <: Sys[S]](obj: Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]]
  }

  private final class Expanded[S <: Sys[S], A](attrView: CellView[S#Tx, Option[A]], tx0: S#Tx)
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
}
final case class ExAttr[A](key: String)(implicit val bridge: ExAttr.Bridge[A])
  extends Ex[Option[A]] with ExAttr.Like[A] {

  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, Option[A]] = {
    ctx.selfOption.fold(Constant(Option.empty[A]).expand[S]) { self =>
      import ctx.targets
      val attrView = bridge.cellView[S](self, key)
      new expr.ExAttr.Expanded[S, A](attrView, tx)
    }
  }

  def aux: scala.List[Aux] = bridge :: Nil
}
