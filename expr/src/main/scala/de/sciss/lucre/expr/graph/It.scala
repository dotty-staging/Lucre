/*
 *  It.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IExpr, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object It {
  trait Expanded[S <: Sys[S], A] extends IExpr[S, A] {
    def setValue(value: A /*, dispatch: Boolean*/)(implicit tx: S#Tx): Unit
  }

  private final class ExpandedImpl[S <: Sys[S], A](implicit protected val targets: ITargets[S])
    extends Expanded[S, A] with IChangeGenerator[S, A] {

    private[this] val ref = Ref.make[A]

    def setValue(value: A /*, dispatch: Boolean*/)(implicit tx: S#Tx): Unit = {
      ref() = value
//      val old =  ref.swap(value)
//      if (/*dispatch && */ old != value) {
//        fire(Change(old, value))
//      }
    }

    private[lucre] def pullChange(pull: IPull[S], isNow: Boolean)(implicit tx: S#Tx): A =
      throw new AssertionError("Should never be here") // pull.resolveChange(isNow = isNow)

//    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
//      Some(pull.resolve)

    def value(implicit tx: S#Tx): A = ref()

    def dispose()(implicit tx: S#Tx): Unit = ()

    def changed: IChangeEvent[S, A] = this
  }
}
/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Ex[A] {

  type Repr[S <: Sys[S]] = It.Expanded[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    new graph.It.ExpandedImpl[S, A]
  }
}
