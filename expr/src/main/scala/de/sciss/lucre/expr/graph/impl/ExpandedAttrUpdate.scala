package de.sciss.lucre.expr
package graph
package impl

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
