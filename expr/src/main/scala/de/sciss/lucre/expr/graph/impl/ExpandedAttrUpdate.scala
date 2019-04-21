/*
 *  ExpandedAttrUpdate.scala
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

import de.sciss.lucre.expr.{CellView, IExpr}
import de.sciss.lucre.stm.{Disposable, Sys}

final class ExpandedAttrUpdate[S <: Sys[S], A](source: IExpr[S, A], attrView: CellView.Var[S, Option[A]], tx0: S#Tx)
  extends Disposable[S#Tx] {

  private[this] val obs = source.changed.react { implicit tx => upd =>
    val value = Some(upd.now)
    attrView.update(value)
  } (tx0)

  def dispose()(implicit tx: S#Tx): Unit =
    obs.dispose()
}
