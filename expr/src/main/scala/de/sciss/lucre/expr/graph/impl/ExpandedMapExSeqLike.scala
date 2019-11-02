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
import de.sciss.lucre.expr.IExpr
import de.sciss.lucre.expr.graph.It
import de.sciss.lucre.stm.Sys

import scala.collection.mutable

abstract class ExpandedMapExSeqLike[S <: Sys[S], A, CC, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                       /* closure: Graph, */ fun: IExpr[S, CC], tx0: S#Tx)
                                                      (implicit protected val targets: ITargets[S]/*, ctx: Context[S]*/)
  extends IExpr[S, Seq[B]] with IChangeEventImpl[S, Seq[B]] {

  in .changed.--->(this)(tx0)
  fun.changed.--->(this)(tx0)
  // this `it` is in the push path and we can wipe the cache
  in. changed.--->(it.changed)(tx0)

  protected def append(b: mutable.Builder[B, _], cc: CC): Unit

  def value(implicit tx: S#Tx): Seq[B] = {
    val inV = in.value
    if (inV.isEmpty) Nil
    else buildResult(inV)(fun.value)
  }

  private def buildResult(inV: Seq[A])(elem: => CC)(implicit tx: S#Tx): Seq[B] = {
    val iterator  = inV.iterator
    val b         = Seq.newBuilder[B]
    b.sizeHint(inV)
    while (iterator.hasNext) {
      val vn = iterator.next()
      it.setValue(vn /*, dispatch = true*/)
      val funV = elem
      append(b, funV)
    }
    b.result()
  }

  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Seq[B] = {
    val inV   = pull.expr(in)
    if (inV.isEmpty) Nil
    else pull.nonCached(it.changed) {
      buildResult(inV)(pull.expr(fun))
    }
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    in .changed.-/->(this)
    fun.changed.-/->(this)
    in. changed.-/->(it.changed)
  }

  def changed: IChangeEvent[S, Seq[B]] = this
}

final class ExpandedMapExSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                /* closure: Graph, */ fun: IExpr[S, B], tx0: S#Tx)
                                               (implicit targets: ITargets[S])
  extends ExpandedMapExSeqLike[S, A, B, B](in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: B): Unit = b += cc
}

final class ExpandedFlatMapExSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                    /* closure: Graph, */ fun: IExpr[S, Seq[B]], tx0: S#Tx)
                                                   (implicit targets: ITargets[S])
  extends ExpandedMapExSeqLike[S, A, Seq[B], B](in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Seq[B]): Unit =
    b ++= cc
}

final class ExpandedFlatMapExSeqOption[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                          /* closure: Graph, */ fun: IExpr[S, Option[B]], tx0: S#Tx)
                                                         (implicit targets: ITargets[S])
  extends ExpandedMapExSeqLike[S, A, Option[B], B](in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Option[B]): Unit = cc match {
    case Some(c)  => b += c
    case None     =>
  }
}
