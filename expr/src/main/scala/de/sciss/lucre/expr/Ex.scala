/*
 *  Ex.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.aux.ProductWithAux
import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}

object Ex {
//  def apply[A](elems: Ex[A]*): Ex[ISeq[A]] = ExSeq[A](elems: _*)

  object Context {
    def apply[S <: Sys[S]](selfH: Option[stm.Source[S#Tx, Obj[S]]] = None): Context[S] =
      new impl.ContextImpl[S](selfH)
  }
  trait Context[S <: Sys[S]] {
    implicit def targets: ITargets[S]

    def selfOption(implicit tx: S#Tx): Option[Obj[S]]

    def visit[U](ref: AnyRef, init: => U)(implicit tx: S#Tx): U
  }

  trait Lazy[A] extends Ex[A] {
    // this acts now as a fast unique reference
    @transient final private[this] lazy val ref = new AnyRef

    final def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A] =
      ctx.visit(ref, mkExpr)

    protected def mkExpr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A]
  }
}
trait Ex[+A] extends ProductWithAux {
  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A]
}
