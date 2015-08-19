package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys

import scala.language.implicitConversions

object Ops {
  implicit def newIntConst[S <: Sys[S]](value: scala.Int): Expr.Const[S, Int] = Int.newConst(value)
  implicit def intExprOps [S <: Sys[S]](ex: Expr[S, Int]): IntExtensions.Ops[S] = new IntExtensions.Ops(ex)
}
