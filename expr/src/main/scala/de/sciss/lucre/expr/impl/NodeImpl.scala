package de.sciss.lucre.expr
package impl

import de.sciss.lucre.event.{impl => evti}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

trait NodeImpl[S <: Sys[S], A]
  extends Expr.Node[S, A]
  with evti.SingleNode[S, Change[A]] { self =>

  // final def changed: Event[S, Change[A]] = this

  // final def disposeData()(implicit tx: S#Tx) = ()

  trait Changed extends evti.SingleEvent[S, Change[A]] {
    def node: Expr.Node[S, A] = self
  }

  override def toString = s"Expr$id"
}