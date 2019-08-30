/*
 *  Context.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.event.{ITargets, Observable}
import de.sciss.lucre.expr.graph.Control
import de.sciss.lucre.expr.impl.ContextImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Disposable, Form, MapLike, NoSys, Obj, Sys, UndoManager, Workspace}

object Context {
  type Attr[S <: Sys[S]] = MapLike[S, String, Form]

  def emptyAttr[S <: Sys[S]]: Attr[S] = anyEmptyAttr.asInstanceOf[EmptyAttr[S]]

  private val anyEmptyAttr = new EmptyAttr[NoSys]

  private final class EmptyAttr[S <: Sys[S]] extends Attr[S] {
    override def toString = "empty"

    def isEmpty (implicit tx: S#Tx): Boolean = true
    def nonEmpty(implicit tx: S#Tx): Boolean = false

    def contains(key: String)(implicit tx: S#Tx): Boolean = false

    def get(key: String)(implicit tx: S#Tx): Option[V] = None

//    def $[R[~ <: Sys[~]] <: Form[~]](key: String)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]] = None

    def changed: Observable[S#Tx, MapLike.Update[S, String, Form]] =
      Observable.empty

    def dispose()(implicit tx: S#Tx): Unit = ()
  }

  def apply[S <: Sys[S]](selfH: Option[stm.Source[S#Tx, Obj[S]]] = None, attr: Attr[S] = emptyAttr[S])
                        (implicit workspace: Workspace[S], cursor: Cursor[S],
                         undoManager: UndoManager[S]): Context[S] =
    new ContextImpl[S](selfH, attr)
}
trait Context[S <: Sys[S]] extends Disposable[S#Tx] {
  implicit def targets    : ITargets    [S]
  implicit def cursor     : Cursor      [S]
  implicit def workspace  : Workspace   [S]
  implicit def undoManager: UndoManager [S]

  def attr/*(implicit tx: S#Tx)*/: Context.Attr[S]

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

