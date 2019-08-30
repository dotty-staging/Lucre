/*
 *  ToTrig.scala
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

import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.expr.{Context, IExpr, ITrigger}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

object ToTrig {
  private final class Expanded[S <: Base[S]](in: IExpr[S, Boolean], tx0: S#Tx)
                                            (implicit protected val targets: ITargets[S])
    extends ITrigger[S] with IEventImpl[S, Unit] {

    in.changed.--->(this)(tx0)

    override def toString: String = s"ToTrig($in)"

    def changed: IEvent[S, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] = {
      val res: Option[Change[Boolean]] = pull(in.changed)
      val t = res.exists(ch => ch.isSignificant && ch.now)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: S#Tx): Unit =
      in.changed -/-> changed
  }
}
final case class ToTrig(in: Ex[Boolean]) extends Trig {
  type Repr[S <: Sys[S]] = ITrigger[S]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val inExp = in.expand[S]
    new ToTrig.Expanded[S](inExp, tx)
  }
}
