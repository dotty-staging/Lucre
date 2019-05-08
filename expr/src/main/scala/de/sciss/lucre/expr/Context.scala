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

  /** Prepares graph expansion by copying control properties over
    * for subsequent look-up through `getProperty`.
    */
  def initGraph(g: Graph)(implicit tx: S#Tx): Unit

  /** Creates a temporary nested context into which all `visit` calls are
    * redirected, thus a compound `Disposable` can be returned.
    */
  def nested[A](body: => A)(implicit tx: S#Tx): (A, Disposable[S#Tx])

  def getProperty[A](control: Control, key: String)(implicit tx: S#Tx): Option[A]

  def selfOption(implicit tx: S#Tx): Option[Obj[S]]

  def visit[U <: Disposable[S#Tx]](ref: AnyRef, init: => U)(implicit tx: S#Tx): U
}

