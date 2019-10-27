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

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.{Ex, It}
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

import scala.collection.mutable
import scala.language.higherKinds

// XXX TODO: DRY
abstract class ExpandedFlatMapExSeqLike[S <: Sys[S], A, B, CC[_]](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                    /* closure: Graph, */ fun: Ex[CC[B]], tx0: S#Tx)
                                                   (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, Seq[B]] with IEventImpl[S, Change[Seq[B]]] {

  in.changed.--->(this)(tx0)

  protected def append(b: mutable.Builder[B, _], cc: CC[B]): Unit

  def value(implicit tx: S#Tx): Seq[B] = {
    val outerV = in.value
    valueOf(outerV)
  }

  private def valueOf(inSeq: Seq[A])(implicit tx: S#Tx): Seq[B] =
    if (inSeq.isEmpty) Nil
    else {
      // XXX TODO --- ok, this is the first test for this idea
      // so we just expand and dispose locally. Later we could
      // optimise to avoid re-expansion for non-empty input sequences.
      val iterator  = inSeq.iterator
      val v0        = iterator.next()
      it.setValue(v0 /*, dispatch = false*/ )  // make sure we have the first value ready
      val (outSeq, funDisposable) = ctx.nested {
        val b     = Seq.newBuilder[B]
        val funEx = fun.expand[S]  // ...which might be read here
        // it.setValue(v0, dispatch = true)
        append(b, funEx.value) // b ++= funEx.value

        // now iterate over the tail
        while (iterator.hasNext) {
          val vn = iterator.next()
          it.setValue(vn /*, dispatch = true*/)
          append(b, funEx.value) // b ++= funEx.value
        }

        b.result()
      }

      funDisposable.dispose()
      outSeq
    }

  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Seq[B]]] =
    pull(in.changed).flatMap { inCh =>
      val before  = valueOf(inCh.before )
      val now     = valueOf(inCh.now    )
      if (before == now) None else Some(Change(before, now))
    }

  def dispose()(implicit tx: S#Tx): Unit =
    in.changed.-/->(this)

  def changed: IEvent[S, Change[Seq[B]]] = this
}

final class ExpandedFlatMapExSeq[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                    /* closure: Graph, */ fun: Ex[Seq[B]], tx0: S#Tx)
                                                   (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedFlatMapExSeqLike(in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Seq[B]): Unit =
    b ++= cc
}

final class ExpandedFlatMapExSeqOption[S <: Sys[S], A, B](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                    /* closure: Graph, */ fun: Ex[Option[B]], tx0: S#Tx)
                                                   (implicit targets: ITargets[S], ctx: Context[S])
  extends ExpandedFlatMapExSeqLike(in, it, fun, tx0) {

  protected def append(b: mutable.Builder[B, _], cc: Option[B]): Unit = cc match {
    case Some(c)  => b += c
    case None     =>
  }
}
