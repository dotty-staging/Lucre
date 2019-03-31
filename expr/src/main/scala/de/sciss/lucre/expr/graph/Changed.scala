/*
 *  Changed.scala
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

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Ex, IExpr, ITrigger, Trig}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

object Changed {
  private final class Expanded[S <: Base[S], A](in: IExpr[S, A], tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S])
    extends ITrigger[S] with IEventImpl[S, Unit] {

    in.changed.--->(this)(tx0)

    override def toString: String = s"Changed($in)"

    def changed: IEvent[S, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] = {
      val res: Option[Change[A]] = pull(in.changed)
      val t = res.exists(_.isSignificant)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: S#Tx): Unit =
      in.changed -/-> changed
  }
}
final case class Changed[A](in: Ex[A]) extends Trig.Lazy {
  protected def mkTrig[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): ITrigger[S] = {
    import ctx.targets
    val inExp = in.expand[S]
    new Changed.Expanded[S, A](inExp, tx)
  }
}
