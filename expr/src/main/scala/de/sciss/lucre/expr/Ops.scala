/*
 *  Ops.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys

import scala.language.implicitConversions

object Ops extends Ops
trait Ops {
  // implicit def newIntConst[S <: Sys[S]](value: Int)(implicit tx: S#Tx): Expr.Const[S, Int] = Int.newConst(value)
  implicit def intObjOps [S <: Sys[S]](obj: IntObj[S]): IntExtensions.Ops[S] = new IntExtensions.Ops(obj)

  // implicit def newLongConst[S <: Sys[S]](value: Long)(implicit tx: S#Tx): Expr.Const[S, Long] = Long.newConst(value)
  implicit def longObjOps [S <: Sys[S]](obj: LongObj[S]): LongExtensions.Ops[S] = new LongExtensions.Ops(obj)

  // implicit def newDoubleConst[S <: Sys[S]](value: Double)(implicit tx: S#Tx): Expr.Const[S, Double] = Double.newConst(value)
  implicit def doubleObjOps [S <: Sys[S]](obj: DoubleObj[S]): DoubleExtensions.Ops[S] = new DoubleExtensions.Ops(obj)

  // implicit def newBooleanConst[S <: Sys[S]](value: Boolean)(implicit tx: S#Tx): Expr.Const[S, Boolean] = Boolean.newConst(value)
  implicit def booleanObjOps [S <: Sys[S]](obj: BooleanObj[S]): BooleanExtensions.Ops[S] = new BooleanExtensions.Ops(obj)

  // implicit def newStringConst[S <: Sys[S]](value: String)(implicit tx: S#Tx): Expr.Const[S, String] = String.newConst(value)
  implicit def stringObjOps [S <: Sys[S]](obj: StringObj[S]): StringExtensions.Ops[S] = new StringExtensions.Ops(obj)

  // implicit def newSpanLikeConst[S <: Sys[S]](value: span.SpanLike)(implicit tx: S#Tx): Expr.Const[S, span.SpanLike] = SpanLike.newConst(value)
  implicit def spanLikeObjOps [S <: Sys[S]](obj: SpanLikeObj[S]): SpanLikeExtensions.Ops[S] = new SpanLikeExtensions.Ops(obj)

  // implicit def newSpanConst[S <: Sys[S]](value: span.Span)(implicit tx: S#Tx): Expr.Const[S, span.Span] = Span.newConst(value)
  implicit def spanObjOps [S <: Sys[S]](obj: SpanObj[S]): SpanExtensions.Ops[S] = new SpanExtensions.Ops(obj)
}