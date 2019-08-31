/*
 *  Span.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.{Context, IExpr, graph}
import de.sciss.lucre.stm.Sys
import de.sciss.span.{Span => _Span}

object Span {
  def apply(start: Ex[Long], stop: Ex[Long]): Ex[_Span] = Apply(start, stop)

  private[lucre] final case class ApplyOp() extends BinaryOp.Op[Long, Long, _Span] {
    override def productPrefix: String = s"Span$$ApplyOp" // serialization

    def apply(a: Long, b: Long): _Span = _Span(a, b)
  }

  private final case class Apply(start: Ex[Long], stop: Ex[Long]) extends Ex[_Span] {
    override def productPrefix: String = "Span" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, _Span]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new graph.BinaryOp.Expanded(ApplyOp(), start.expand[S], stop.expand[S], tx)
    }
  }
}
