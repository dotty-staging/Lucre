/*
 *  ExpandedMapExSeq.scala
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

import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.It
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

final class ExpandedMapExSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                /* closure: Graph, */ fun: IExpr[S, B], tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, Seq[B]] with IChangeEventImpl[S, Seq[B]] {

  in .changed.--->(this)(tx0)
  fun.changed.--->(this)(tx0)

  def value(implicit tx: S#Tx): Seq[B] = {
    val inV = in.value
    valueOf(inV)
  }

  private def valueOf(inV: Seq[A])(implicit tx: S#Tx): Seq[B] =
    if (inV.isEmpty) Nil
    else {
      val iterator  = inV.iterator
      val b         = Seq.newBuilder[B]
      while (iterator.hasNext) {
        val vn = iterator.next()
        it.setValue(vn /*, dispatch = true*/)
        val funV = fun.value
        b += funV
      }
      b.result()
    }

  private[lucre] def pullChange(pull: IPull[S], isNow: Boolean)(implicit tx: S#Tx): Seq[B] = {
    val inCh  = in.changed
    val inV   = if (pull.contains(inCh)) pull.applyChange(inCh, isNow = isNow) else in.value
//    valueOf(inV)
    if (inV.isEmpty) Nil
    else {
      val iterator  = inV.iterator
      val b         = Seq.newBuilder[B]
      val funCh     = fun.changed
      val funP      = pull.contains(funCh)
      while (iterator.hasNext) {
        val vn = iterator.next()
        it.setValue(vn /*, dispatch = true*/)
        val funV = if (funP) pull.applyChange(funCh, isNow = isNow) else fun.value
        b += funV
        pull.TEST_PURGE_CACHE()
      }
      b.result()
    }
  }

  private[lucre] def pullUpdateXXX(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Seq[B]]] =
    pull(in.changed).flatMap { inCh =>
      val before  = valueOf(inCh.before )
      val now     = valueOf(inCh.now    )
      if (before == now) None else Some(Change(before, now))
    }

  def dispose()(implicit tx: S#Tx): Unit = {
    in .changed.-/->(this)
    fun.changed.-/->(this)
  }

  def changed: IChangeEvent[S, Seq[B]] = this
}