/*
 *  ExpandedFlatMapExOption.scala
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

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

// XXX TODO DRY with ExpandedMapExOption
final class ExpandedFlatMapExOption[S <: Sys[S], A, B](in: IExpr[S, Option[A]], fun: Ex[Option[B]], tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S], ctx: Context[S])
  extends IExpr[S, Option[B]] with IEventImpl[S, Change[Option[B]]] {

  in.changed.--->(this)(tx0)

  private[this] val ref = Ref(valueOf(in.value(tx0))(tx0))

  def value(implicit tx: S#Tx): Option[B] = ref()._1

  private def valueOf(inOption: Option[A])(implicit tx: S#Tx): (Option[B], Disposable[S#Tx]) =
    if (inOption.isEmpty) (None, Disposable.empty[S#Tx]) else {
      val tup = ctx.nested {
        val funEx = fun.expand[S]
        val vn    = funEx.value
        vn
      }

      tup
    }

  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[B]]] =
    pull(in.changed).flatMap { inCh =>
      val beforeTup = ref()
      beforeTup._2.dispose()
      val before    = beforeTup._1
      val nowTup    = valueOf(inCh.now)
      ref() = nowTup
      val now       = nowTup._1
      if (before == now) None else Some(Change(before, now))
    }

  def dispose()(implicit tx: S#Tx): Unit = {
    ref.swap((None, Disposable.empty[S#Tx]))._2.dispose()
    in.changed.-/->(this)
  }

  def changed: IEvent[S, Change[Option[B]]] = this
}
