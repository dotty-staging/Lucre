package de.sciss.lucre.expr.impl

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys, TxnLike}

import scala.concurrent.stm.TMap

trait ContextMixin[S <: Sys[S]] extends Context[S] {
  final val targets: ITargets[S] = ITargets[S]

  private[this] val sourceMap = TMap.empty[AnyRef, Any]

  final def visit[U](ref: AnyRef, init: => U)(implicit tx: S#Tx): U = {
    import TxnLike.peer
    sourceMap.get(ref) match {
      case Some(res) => res.asInstanceOf[U]  // not so pretty...
      case None =>
        val exp    = init
        sourceMap += ref -> exp
        exp
    }
  }
}

final class ContextImpl[S <: Sys[S]](ownerH: stm.Source[S#Tx, Obj[S]]) extends ContextMixin[S] {
  def self(implicit tx: S#Tx): Obj[S] = ownerH()
}