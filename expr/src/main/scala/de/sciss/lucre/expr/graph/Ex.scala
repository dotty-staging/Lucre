/*
 *  Ex.scala
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

import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys

object Ex {
  trait Lazy[A] extends Ex[A] {
    // this acts now as a fast unique reference
    @transient final private[this] lazy val ref = new AnyRef

    final def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A] =
      ctx.visit(ref, mkExpr)

    protected def mkExpr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A]
  }
}
trait Ex[+A] extends Product {
  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A]
}
