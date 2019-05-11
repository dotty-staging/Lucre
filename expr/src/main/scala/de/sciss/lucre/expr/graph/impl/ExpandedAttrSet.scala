/*
 *  ExpandedAttrSet.scala
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

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{CellView, IExpr}
import de.sciss.lucre.stm.Sys

final class ExpandedAttrSet[S <: Sys[S], A](source: IExpr[S, A], attrView: CellView.Var[S, Option[A]], tx0: S#Tx)
  extends IActionImpl[S] {

  def executeAction()(implicit tx: S#Tx): Unit = {
    val v = source.value
    attrView.update(Some(v))
  }
}
