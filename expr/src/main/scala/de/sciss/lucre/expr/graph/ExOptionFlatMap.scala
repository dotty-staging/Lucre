/*
 *  ExOptionFlatMap.scala
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
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

object ExOptionFlatMap {
  // XXX DRY with ExOptionMap
  private final class Expanded[S <: Sys[S], A, B](in: IExpr[S, Option[A]], fun: Ex[Option[B]], tx0: S#Tx)
                                                 (implicit protected val targets: ITargets[S], ctx: Context[S])
    extends IExpr[S, Option[B]] with IEventImpl[S, Change[Option[B]]] {

    in.changed.--->(this)(tx0)

    def value(implicit tx: S#Tx): Option[B] = {
      val outerV = in.value
      valueOf(outerV)
    }

    private def valueOf(inOption: Option[A])(implicit tx: S#Tx): Option[B] =
      if (inOption.isEmpty) None else {
        val (out, funDisp) = ctx.nested {
          val funEx = fun.expand[S]
          val vn    = funEx.value
          vn
        }

        funDisp.dispose()
        out
      }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Option[B]]] =
      pull(in.changed).flatMap { inCh =>
        val before  = valueOf(inCh.before )
        val now     = valueOf(inCh.now    )
        if (before == now) None else Some(Change(before, now))
      }

    def dispose()(implicit tx: S#Tx): Unit =
      in.changed.-/->(this)

    def changed: IEvent[S, Change[Option[B]]] = this
  }
}
final case class ExOptionFlatMap[A, B](in: Ex[Option[A]], fun: Ex[Option[B]])
  extends Ex[Option[B]] {

  type Repr[S <: Sys[S]] = IExpr[S, Option[B]]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val inEx = in.expand[S]
    import ctx.targets
    new ExOptionFlatMap.Expanded[S, A, B](inEx, fun, tx)
  }
}
