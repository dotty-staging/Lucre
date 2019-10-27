/*
 *  ExpandedMapActOption.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.Act
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr}
import de.sciss.lucre.stm.Sys

final class ExpandedMapActOption[S <: Sys[S], A](in: IExpr[S, Option[A]], fun: Act, tx0: S#Tx)
                                                (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IAction.Option[S] with IActionImpl[S] {

  def isDefined(implicit tx: S#Tx): Boolean = in.value.isDefined

  def executeAction()(implicit tx: S#Tx): Unit = executeIfDefined()

  def executeIfDefined()(implicit tx: S#Tx): Boolean = {
    val inOption = in.value
    inOption.isDefined && {
      val (_, d) = ctx.nested {
        val actEx = fun.expand[S]
        actEx.executeAction()
      }

      d.dispose()
      true
    }
  }

//  private def valueOf(inOption: Option[A])/*(implicit tx: S#Tx)*/: Option[Act] =
//    if (inOption.isDefined) Some(fun) else None

//  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[Act]]] =
//    pull(in.changed).flatMap { inCh =>
//      val before  = valueOf(inCh.before )
//      val now     = valueOf(inCh.now    )
//      if (before == now) None else Some(Change(before, now))
//    }

//  def dispose()(implicit tx: S#Tx): Unit =
//    in.changed.-/->(this)

//  def changed: IEvent[S, Change[Option[Act]]] = this
}
