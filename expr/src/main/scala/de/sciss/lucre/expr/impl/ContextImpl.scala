/*
 *  ContextMixin.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.Control
import de.sciss.lucre.expr.{Context, Graph}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, Sys, UndoManager, Workspace}

import scala.annotation.tailrec
import scala.concurrent.stm.{Ref, TMap}

trait ContextMixin[S <: Sys[S]] extends Context[S] {
  // ---- abstract ----

  protected def selfH: Option[stm.Source[S#Tx, Obj[S]]]

  // ---- impl ----

  final val targets: ITargets[S] = ITargets[S]

  private type SourceMap  = Map [AnyRef, Disposable[S#Tx]]
  private type Properties = TMap[AnyRef, Map[String, Any]]

  private[this] val sourceMap : Ref[SourceMap]  = Ref(Map.empty)
  private[this] val properties: Properties      = TMap.empty

  private[this] val parents = Ref(List.empty[SourceMap])

  def initGraph(g: Graph)(implicit tx: S#Tx): Unit = {
    properties.clear()
    g.controls.foreach { conf =>
      properties.put(conf.control.token, conf.properties)
    }
  }

  def nested[A](body: => A)(implicit tx: S#Tx): (A, Disposable[S#Tx]) = {
    val parentMap = sourceMap.swap(Map.empty)
    parents.transform(parentMap :: _)
    val res = body
    val disposableIt = sourceMap().values
    val disposable: Disposable[S#Tx] =
      if (disposableIt.isEmpty) Disposable.empty
      else disposableIt.toList match {
        case single :: Nil  => single
        case more           => Disposable.seq(more: _*)
      }

    parents.transform(_.tail)
    sourceMap() = parentMap

    (res, disposable)
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    require (parents().isEmpty, "Must not call dispose in a nested operation")
    val m = sourceMap.swap(Map.empty)
    m.foreach(_._2.dispose())
  }

  final def visit[U <: Disposable[S#Tx]](ref: AnyRef, init: => U)(implicit tx: S#Tx): U = {
    sourceMap().get(ref) match {
      case Some(res) => res.asInstanceOf[U]  // not so pretty...
      case None =>
        @tailrec
        def loop(rem: List[SourceMap]): U =
          rem match {
            case head :: tail =>
              head.get(ref) match {
                case Some(res) => res.asInstanceOf[U]
                case None => loop(tail)
              }

            case Nil =>
              val exp    = init
              sourceMap.transform(m => m + (ref -> exp))
              exp
          }

        loop(parents())
     }
  }

  def selfOption(implicit tx: S#Tx): Option[Obj[S]] = selfH.map(_.apply())

  def getProperty[A](c: Control, key: String)(implicit tx: S#Tx): Option[A] = {
    val m0: Map[String, Any] = properties.get(c.token).orNull
    if (m0 == null) None else {
      m0.get(key).asInstanceOf[Option[A]]
    }
  }
}

final class ContextImpl[S <: Sys[S]](protected val selfH: Option[stm.Source[S#Tx, Obj[S]]],
                                     val attr: Context.Attr[S])
                                    (implicit val workspace: Workspace[S], val cursor: Cursor[S],
                                     val undoManager: UndoManager[S])
  extends ContextMixin[S]