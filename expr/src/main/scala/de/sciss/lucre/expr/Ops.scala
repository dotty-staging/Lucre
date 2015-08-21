/*
 *  Ops.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys
import de.sciss.span

import scala.language.implicitConversions

object Ops {
  implicit def newIntConst[S <: Sys[S]](value: Int)(implicit tx: S#Tx): Expr.Const[S, Int] = Int.newConst(value)
  implicit def intExprOps [S <: Sys[S]](ex: Expr[S, Int]): IntExtensions.Ops[S] = new IntExtensions.Ops(ex)

  implicit def newLongConst[S <: Sys[S]](value: Long)(implicit tx: S#Tx): Expr.Const[S, Long] = Long.newConst(value)
  implicit def longExprOps [S <: Sys[S]](ex: Expr[S, Long]): LongExtensions.Ops[S] = new LongExtensions.Ops(ex)

  implicit def newDoubleConst[S <: Sys[S]](value: Double)(implicit tx: S#Tx): Expr.Const[S, Double] = Double.newConst(value)
  implicit def doubleExprOps [S <: Sys[S]](ex: Expr[S, Double]): DoubleExtensions.Ops[S] = new DoubleExtensions.Ops(ex)

  implicit def newBooleanConst[S <: Sys[S]](value: Boolean)(implicit tx: S#Tx): Expr.Const[S, Boolean] = Boolean.newConst(value)
  implicit def booleanExprOps [S <: Sys[S]](ex: Expr[S, Boolean]): BooleanExtensions.Ops[S] = new BooleanExtensions.Ops(ex)

  implicit def newStringConst[S <: Sys[S]](value: String)(implicit tx: S#Tx): Expr.Const[S, String] = String.newConst(value)
  implicit def stringExprOps [S <: Sys[S]](ex: Expr[S, String]): StringExtensions.Ops[S] = new StringExtensions.Ops(ex)

  implicit def newSpanLikeConst[S <: Sys[S]](value: span.SpanLike)(implicit tx: S#Tx): Expr.Const[S, span.SpanLike] = SpanLike.newConst(value)
  implicit def spanLikeExprOps [S <: Sys[S]](ex: Expr[S, span.SpanLike]): SpanLikeExtensions.Ops[S] = new SpanLikeExtensions.Ops(ex)

  implicit def newSpanConst[S <: Sys[S]](value: span.Span)(implicit tx: S#Tx): Expr.Const[S, span.Span] = Span.newConst(value)
  implicit def spanExprOps [S <: Sys[S]](ex: Expr[S, span.Span]): SpanExtensions.Ops[S] = new SpanExtensions.Ops(ex)
}