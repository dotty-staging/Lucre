/*
 *  ExpandedMapExOption.scala
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
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys

//final class ExpandedMapExOption[S <: Sys[S], A, B](in: IExpr[S, Option[A]], fun: Ex[B], tx0: S#Tx)
//                                                  (implicit protected val targets: ITargets[S], ctx: Context[S])
//  extends IExpr[S, Option[B]] with IEventImpl[S, Change[Option[B]]] {
//
//  in.changed.--->(this)(tx0)
//
//  def value(implicit tx: S#Tx): Option[B] = {
//    val outerV = in.value
//    valueOf(outerV)
//  }
//
//  private def valueOf(inOption: Option[A])(implicit tx: S#Tx): Option[B] =
//    if (inOption.isEmpty) None else {
//      val (out, d) = ctx.nested {
//        val funEx = fun.expand[S]
//        val vn    = funEx.value
//        vn
//      }
//
//      d.dispose()
//      Some(out)
//    }
//
//  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[B]]] =
//    pull(in.changed).flatMap { inCh =>
//      val before  = valueOf(inCh.before )
//      val now     = valueOf(inCh.now    )
//      if (before == now) None else Some(Change(before, now))
//    }
//
//  def dispose()(implicit tx: S#Tx): Unit =
//    in.changed.-/->(this)
//
//  def changed: IEvent[S, Change[Option[B]]] = this
//}

// XXX TODO --- why is this using `Caching`?
// Note: this needs `Caching` because the closure `fun` may depend on expressions from the
// environment, and therefore... XXX TODO: we need to track fun.expand.changed
final class ExpandedMapExOption[S <: Sys[S], A, B](in: IExpr[S, Option[A]], fun: IExpr[S, B], tx0: S#Tx)
                                                  (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, Option[B]] with IChangeEventImpl[S, Option[B]] {

  in  .changed.--->(this)(tx0)
  fun .changed.--->(this)(tx0)

  override def toString: String = s"$in.map($fun)"

  def value(implicit tx: S#Tx): Option[B] = {
    if (in.value.isDefined) Some(fun.value) else None
  }

//  private def valueOf(inOption: Option[A])(implicit tx: S#Tx): Option[(B, Disposable[S#Tx])] =
//    if (inOption.isDefined) None else {
//      val tup = ctx.nested {
//        val funEx = fun.expand[S]
//        val vn    = funEx.value
//        vn
//      }
//
//      Some(tup)
//    }

  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Option[B] = {
    val inV: Option[A] = pull.expr(in)
    if (inV.isEmpty) None else {
      val funV: B = pull.expr(fun)
      Some(funV)
    }
  }

//  private[lucre] def pullUpdateXXX(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[B]]] =
//    pull(in.changed).flatMap { inCh =>
//      val beforeTup = ref()
//      beforeTup.foreach(_._2.dispose())
//      val before    = beforeTup.map(_._1)
//      val nowTup    = valueOf(inCh.now)
//      ref() = nowTup
//      val now       = nowTup.map(_._1)
//      if (before == now) None else Some(Change(before, now))
//    }

  def dispose()(implicit tx: S#Tx): Unit = {
    in  .changed.-/->(this)
    fun .changed.-/->(this)
  }

  def changed: IChangeEvent[S, Option[B]] = this
}
