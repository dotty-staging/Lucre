package de.sciss.lucre.expr
package impl

import de.sciss.lucre.event.{impl => evti}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

trait NodeImpl[S <: Sys[S], A]
  extends Expr.Node[S, A]
  with evti.SingleNode[S, Change[A]] {

  // final def changed: Event[S, Change[A]] = this

  final def disposeData()(implicit tx: S#Tx) = ()

  override def toString = s"Expr$id"
}