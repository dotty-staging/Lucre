/*
 *  ContextMixin.scala
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
package impl

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.Control
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, Source, Sys, Workspace}

import scala.concurrent.stm.TMap

trait ContextMixin[S <: Sys[S]] extends Context[S] {
  // ---- abstract ----

  type Repr <: ContextMixin[S]

  protected def selfH: Option[stm.Source[S#Tx, Obj[S]]]

  // ---- impl ----

//  final val targets: ITargets[S] = ITargets[S]

  private[this] val sourceMap   = TMap.empty[AnyRef, Disposable[S#Tx]]
  private[this] val properties  = TMap.empty[AnyRef, Map[String, Any]]

  protected def parentOption: Option[Repr]

  protected def mkChild: Repr

  def initGraph(g: Graph)(implicit tx: S#Tx): Unit = {
    properties.clear()
    g.controls.foreach { conf =>
      properties.put(conf.control.token, conf.properties)
    }
  }

  def nested[A](body: => A)(implicit tx: S#Tx): (A, Disposable[S#Tx]) = {
    ??? // continue here
    val disposableIt = sourceMap.values
    val disposable: Disposable[S#Tx] =
      if (disposableIt.isEmpty) Disposable.empty
      else disposableIt.toList match {
        case single :: Nil  => single
        case more           => Disposable.seq(more: _*)
      }

    (body, disposable)
  }

  def dispose()(implicit tx: S#Tx): Unit =
    if (!sourceMap.isEmpty) {
      sourceMap.foreach(_._2.dispose())
      sourceMap.clear()
    }

  final def visit[U <: Disposable[S#Tx]](ref: AnyRef, init: => U)(implicit tx: S#Tx): U = {
    sourceMap.get(ref) match {
      case Some(res) => res.asInstanceOf[U]  // not so pretty...
      case None =>
        val exp    = init
        sourceMap += ref -> exp
        exp
    }
  }

  def selfOption(implicit tx: S#Tx): Option[Obj[S]] = selfH.map(_.apply())

  def getProperty[A](c: Control, key: String)(implicit tx: S#Tx): Option[A] = {
    val m0 = properties.get(c.token)
    if (m0 == null) None else {
      m0.get(key).asInstanceOf[Option[A]]
    }
  }
}

final class ContextImpl[S <: Sys[S]](protected val selfH: Option[stm.Source[S#Tx, Obj[S]]])
                                    (implicit val workspace: Workspace[S], val cursor: Cursor[S])
  extends ContextMixin[S] { outer =>

  type Repr = ContextMixin[S]

  val targets: ITargets[S] = ITargets[S]

  protected def parentOption: Option[ContextImpl[S]] = None

  protected def mkChild: Repr = new Child(this)

  private final class Child(parent: ContextMixin[S]) extends ContextMixin[S] {
    type Repr = ContextMixin[S]

    protected def selfH: Option[Source[S#Tx, Obj[S]]] = outer.selfH

    protected val parentOption: Option[Repr] = Some(parent)

    protected def mkChild: Repr = new Child(this)

    implicit def targets  : ITargets  [S] = outer.targets
    implicit def cursor   : Cursor    [S] = outer.cursor
    implicit def workspace: Workspace [S] = outer.workspace
  }
}