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

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.expr.impl.ContextImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Obj, Sys, Workspace}

object Ex {
  object Context {
    def apply[S <: Sys[S]](g: Graph = Graph.empty, selfH: Option[stm.Source[S#Tx, Obj[S]]] = None)
                          (implicit workspace: Workspace[S], cursor: Cursor[S]): Context[S] =
      new ContextImpl[S](g, selfH)
  }
  trait Context[S <: Sys[S]] {
    implicit def targets  : ITargets        [S]
    implicit def cursor   : Cursor          [S]
    implicit def workspace: Workspace [S]

    def getProperty[A](c: Control, key: String): Option[A]

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
trait Ex[+A] extends Product {
  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, A]
}
