/*
 *  ExMap.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Ex, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

import scala.collection.immutable.{Seq => ISeq}

object ExMap {
  private final class Expanded[S <: Sys[S], A, B](outer: IExpr[S, ISeq[A]], it: It.Expanded[S, A],
                                                  inner: IExpr[S, B], tx0: S#Tx)
                                                 (implicit protected val targets: ITargets[S])
    extends IExpr[S, ISeq[B]] with IEventImpl[S, Change[ISeq[B]]] {

    outer.changed.--->(this)(tx0)

    def value(implicit tx: S#Tx): ISeq[B] = {
      val outerV = outer.value
      valueOf(outerV)
    }

    private def valueOf(inSeq: ISeq[A])(implicit tx: S#Tx): ISeq[B] =
      inSeq.map { in =>
        it.setValue(in)
        inner.value
      }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[ISeq[B]]] =
      pull(outer.changed).flatMap { inCh =>
        val before  = valueOf(inCh.before )
        val now     = valueOf(inCh.now    )
        if (before == now) None else Some(Change(before, now))
      }

    def dispose()(implicit tx: S#Tx): Unit =
      outer.changed.-/->(this)

    def changed: IEvent[S, Change[ISeq[B]]] = this
  }
}
final case class ExMap[A, B](outer: Ex[ISeq[A]], it: It[A], inner: Ex[B]) extends Ex.Lazy[ISeq[B]] {
  protected def mkExpr[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, ISeq[B]] = {
    val outerEx = outer .expand[S]
    val itEx    = it    .expand[S]
    val innerEx = inner .expand[S]
    import ctx.targets
    new ExMap.Expanded[S, A, B](outerEx, itEx, innerEx, tx)
  }
}
