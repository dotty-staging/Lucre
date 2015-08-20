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

import scala.language.implicitConversions

object Ops {
  implicit def newIntConst[S <: Sys[S]](value: scala.Int)(implicit tx: S#Tx): Expr.Const[S, Int] = Int.newConst(value)
  implicit def intExprOps [S <: Sys[S]](ex: Expr[S, Int]): IntExtensions.Ops[S] = new IntExtensions.Ops(ex)
}
