/*
 *  It.scala
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

import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.{IExpr, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object It {
  trait Expanded[S <: Sys[S], A] extends IExpr[S, A] {
    def setValue(value: A)(implicit tx: S#Tx): Unit
  }

  private final class ExpandedImpl[S <: Sys[S], A](implicit protected val targets: ITargets[S])
    extends Expanded[S, A] with IGenerator[S, Change[A]] {

    private[this] val ref = Ref.make[A]

    def setValue(value: A)(implicit tx: S#Tx): Unit = {
      val old = ref.swap(value)
      if (old != value) {
        fire(Change(old, value))
      }
    }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
      Some(pull.resolve)

    def value(implicit tx: S#Tx): A = ref()

    def dispose()(implicit tx: S#Tx): Unit = ()

    def changed: IEvent[S, Change[A]] = this
  }
}
/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Ex[A] {
  // this acts now as a fast unique reference
  @transient final private[this] lazy val ref = new AnyRef

  override def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): It.Expanded[S, A] =
    ctx.visit(ref, mkExpr)

  private def mkExpr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): It.Expanded[S, A] = {
    import ctx.targets
    new graph.It.ExpandedImpl[S, A]
  }
}
