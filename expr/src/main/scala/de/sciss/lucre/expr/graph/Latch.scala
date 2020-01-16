/*
 *  Latch.scala
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

import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, IPush, ITargets}
import de.sciss.lucre.expr.{Context, IExpr, ITrigger, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer

import scala.concurrent.stm.Ref

object Latch {
  private final class Expanded[S <: Sys[S], A](in: IExpr[S, A], trig: ITrigger[S], tx0: S#Tx)
                                              (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IChangeEventImpl[S, A] with Caching {

    private[this] val ref = Ref(in.value(tx0))

    trig.changed.--->(this)(tx0)

    def value(implicit tx: S#Tx): A =
      IPush.tryPull(this).fold(ref())(_.now)

    def dispose()(implicit tx: S#Tx): Unit =
      trig.changed.-/->(this)

    def changed: IChangeEvent[S, A] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): A =
      if (phase.isBefore || pull(trig.changed).isEmpty) ref() else {
        val newValue  = in.value
        ref()         = newValue
        newValue
      }

//    private[lucre] def pullUpdateXXX(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
//      if (pull(trig.changed).isEmpty) None else {
//        val newValue  = in.value
//        val oldValue  = ref.swap(newValue)
//        if (oldValue == newValue) None else Some(Change(oldValue, newValue))
//      }
  }
}
/** Latches the expression based on the trigger argument.
  * The initial state of the returned expression corresponds to the
  * initial state of the input expression. Subsequent values are
  * updated and cached only when a trigger occurs.
  */
final case class Latch[A](in: Ex[A], trig: Trig) extends Ex[A] {
  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    new graph.Latch.Expanded(in.expand[S], trig.expand[S], tx)
  }
}
