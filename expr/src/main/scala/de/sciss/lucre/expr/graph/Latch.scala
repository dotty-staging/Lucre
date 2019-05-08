/*
 *  Latch.scala
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

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{Caching, IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IExpr, ITrigger, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object Latch {
  private final class Expanded[S <: Sys[S], A](init: IExpr[S, A], trig: ITrigger[S], tx0: S#Tx)
                                              (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IEventImpl[S, Change[A]] with Caching {

    private[this] val ref = Ref(init.value(tx0))

    trig.changed.--->(this)(tx0)

    def value(implicit tx: S#Tx): A = ref()

    def dispose()(implicit tx: S#Tx): Unit =
      trig.changed.-/->(this)

    def changed: IEvent[S, Change[A]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
      if (pull(trig.changed).isEmpty) None else {
        val newValue  = init.value
        val oldValue  = ref.swap(newValue)
        if (oldValue == newValue) None else Some(Change(oldValue, newValue))
      }
    }
  }
}
final case class Latch[A](in: Ex[A], trig: Trig) extends Ex[A] {
  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    new graph.Latch.Expanded(in.expand[S], trig.expand[S], tx)
  }
}
