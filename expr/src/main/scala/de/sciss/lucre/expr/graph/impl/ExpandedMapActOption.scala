/*
 *  ExpandedMapActOption.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.expr.graph.{Act, It}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

final class ExpandedMapActOption[S <: Sys[S], A](in: IExpr[S, Option[A]], it: It.Expanded[S, A],
                                                 fun: Act, tx0: S#Tx)
                                                (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, Option[Act]] with IEventImpl[S, Change[Option[Act]]] with Act.Option[S] {

  in.changed.--->(this)(tx0)

  def value(implicit tx: S#Tx): Option[Act] = {
    val outerV = in.value
    valueOf(outerV)
  }

  def executeAction()(implicit tx: S#Tx): Boolean = {
    val inOption = in.value
    inOption.exists { v0 =>
      it.setValue(v0)  // make sure we have the first value ready
      val (_, d) = ctx.nested {
        val actEx = fun.expand[S]
        actEx.executeAction()
      }

      d.dispose()
      true
    }
  }

  private def valueOf(inOption: Option[A])/*(implicit tx: S#Tx)*/: Option[Act] =
    if (inOption.isDefined) Some(fun) else None

  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[Act]]] =
    pull(in.changed).flatMap { inCh =>
      val before  = valueOf(inCh.before )
      val now     = valueOf(inCh.now    )
      if (before == now) None else Some(Change(before, now))
    }

  def dispose()(implicit tx: S#Tx): Unit =
    in.changed.-/->(this)

  def changed: IEvent[S, Change[Option[Act]]] = this
}
