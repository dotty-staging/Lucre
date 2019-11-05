/*
 *  ExpandedMapOptionAct.scala
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
import de.sciss.lucre.expr.graph.{Act, It}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr}
import de.sciss.lucre.stm.Sys

final class ExpandedMapOptionAct[S <: Sys[S], A](in: IExpr[S, Option[A]], it: It.Expanded[S, A], fun: Act, tx0: S#Tx)
                                                (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IAction.Option[S] with IActionImpl[S] {

  def isDefined(implicit tx: S#Tx): Boolean = in.value.isDefined

  def executeAction()(implicit tx: S#Tx): Unit = executeIfDefined()

  def executeIfDefined()(implicit tx: S#Tx): Boolean = {
    val inOption = in.value
    inOption.exists { v =>
      it.setValue(v)
      val (_, d) = ctx.nested(it) {
        val actEx = fun.expand[S]
        actEx.executeAction()
      }

      d.dispose()
      true
    }
  }
}
