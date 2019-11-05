/*
 *  ExpandedMapSeqAct.scala
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

final class ExpandedMapSeqAct[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                              fun: Act, tx0: S#Tx)
                                             (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IAction[S] with IActionImpl[S] {

  def executeAction()(implicit tx: S#Tx): Unit = {
    val inV = in.value
    if (inV.isEmpty) return

    val iterator  = inV.iterator
    while (iterator.hasNext) {
      val vn = iterator.next()
      it.setValue(vn)
      val (_, d) = ctx.nested(it) {
        val funEx = fun.expand[S] // ...which might be read here
        funEx.executeAction()
        ()
      }
      d.dispose()
    }
  }
}