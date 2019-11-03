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

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.{Act, It}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr}
import de.sciss.lucre.stm.Sys

final class ExpandedMapExSeqAct[S <: Sys[S], A](in: IExpr[S, Seq[A]], it: It.Expanded[S, A],
                                                /* closure: Graph, */ fun: Act, tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IAction[S] /*with IEventImpl[S, Change[Seq[Act]]]*/ with IActionImpl[S] {

  def executeAction()(implicit tx: S#Tx): Unit = {
    val inV = in.value
    if (inV.isEmpty) return

    val iterator  = inV.iterator
    val v0        = iterator.next()
    it.setValue(v0 /*, dispatch = false*/ )  // make sure we have the first value ready
    val (_, funDisposable) = ctx.nested(it) {
      val funEx = fun.expand[S]  // ...which might be read here
      // it.setValue(v0, dispatch = true)
      funEx.executeAction()

      // now iterate over the tail
      while (iterator.hasNext) {
        val vn = iterator.next()
        it.setValue(vn /*, dispatch = true*/)
        funEx.executeAction()
      }

      ()
    }

    funDisposable.dispose()
  }

  //  in.changed.--->(this)(tx0)

//  def value(implicit tx: S#Tx): Seq[Act] = {
//    val outerV = in.value
//    valueOf(outerV)
//  }
//
//  private def valueOf(inSeq: Seq[A])(implicit tx: S#Tx): Seq[Act] =
//    if (inSeq.isEmpty) Nil
//    else {
//      // XXX TODO --- ok, this is the first test for this idea
//      // so we just expand and dispose locally. Later we could
//      // optimise to avoid re-expansion for non-empty input sequences.
//      val iterator  = inSeq.iterator
//      val v0        = iterator.next()
//      it.setValue(v0 /*, dispatch = false*/ )  // make sure we have the first value ready
//      val (outSeq, funDisposable) = ctx.nested {
//        val b     = Seq.newBuilder[Act]
//        val funEx = fun.expand[S]  // ...which might be read here
//        // it.setValue(v0, dispatch = true)
//        b += funEx.value
//
//        // now iterate over the tail
//        while (iterator.hasNext) {
//          val vn = iterator.next()
//          it.setValue(vn /*, dispatch = true*/)
//          b += funEx.value
//        }
//
//        b.result()
//      }
//
//      funDisposable.dispose()
//      outSeq
//    }

//  private def valueOf(inSeq: Seq[A])(implicit tx: S#Tx): Seq[B] =
//    if (inSeq.isEmpty) Nil
//    else {
//      // XXX TODO --- ok, this is the first test for this idea
//      // so we just expand and dispose locally. Later we could
//      // optimise to avoid re-expansion for non-empty input sequences.
//      val iterator  = inSeq.iterator
//      val v0        = iterator.next()
//      it.setValue(v0 /*, dispatch = false*/ )  // make sure we have the first value ready
//      val (outSeq, funDisposable) = ctx.nested {
//        val b     = Seq.newBuilder[B]
//        val funEx = fun.expand[S]  // ...which might be read here
//        // it.setValue(v0, dispatch = true)
//        b += funEx.value
//
//        // now iterate over the tail
//        while (iterator.hasNext) {
//          val vn = iterator.next()
//          it.setValue(vn /*, dispatch = true*/)
//          b += funEx.value
//        }
//
//        b.result()
//      }
//
//      funDisposable.dispose()
//      outSeq
//    }

//  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Seq[B]]] =
//    pull(in.changed).flatMap { inCh =>
//      val before  = valueOf(inCh.before )
//      val now     = valueOf(inCh.now    )
//      if (before == now) None else Some(Change(before, now))
//    }

//  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx) : Option[Change[Seq[Act]]] = ???
//
//  def dispose()(implicit tx: S#Tx): Unit =
//    in.changed.-/->(this)
//
//  def changed: IEvent[S, Change[Seq[Act]]] = this
}