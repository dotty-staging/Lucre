/*
 *  Context.scala
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

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.Control
import de.sciss.lucre.expr.impl.ContextImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, Sys, Workspace}

object Context {
  def apply[S <: Sys[S]](selfH: Option[stm.Source[S#Tx, Obj[S]]] = None)
                        (implicit workspace: Workspace[S], cursor: Cursor[S]): Context[S] =
    new ContextImpl[S](selfH)
}
trait Context[S <: Sys[S]] extends Disposable[S#Tx] {
  implicit def targets  : ITargets  [S]
  implicit def cursor   : Cursor    [S]
  implicit def workspace: Workspace [S]

  def withGraph[A](g: Graph)(body: => A)(implicit tx: S#Tx): (A, Disposable[S#Tx])

  def getProperty[A](c: Control, key: String): Option[A]

  def selfOption(implicit tx: S#Tx): Option[Obj[S]]

  def visit[U <: Disposable[S#Tx]](ref: AnyRef, init: => U)(implicit tx: S#Tx): U
}

