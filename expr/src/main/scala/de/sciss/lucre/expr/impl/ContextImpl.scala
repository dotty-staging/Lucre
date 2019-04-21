/*
 *  ContextImpl.scala
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

import java.util

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Obj, Sys, TxnLike, Workspace}

import scala.concurrent.stm.TMap

trait ContextMixin[S <: Sys[S]] extends Context[S] {
  // ---- abstract ----
  protected val graph: Graph
  protected def selfH: Option[stm.Source[S#Tx, Obj[S]]]

  // ---- impl ----

  final val targets: ITargets[S] = ITargets[S]

  private[this] val sourceMap   = TMap.empty[AnyRef, Any]
  private[this] val properties  = new util.IdentityHashMap[Control, Map[String, Any]]()

  final def visit[U](ref: AnyRef, init: => U)(implicit tx: S#Tx): U = {
    import TxnLike.peer
    sourceMap.get(ref) match {
      case Some(res) => res.asInstanceOf[U]  // not so pretty...
      case None =>
        val exp    = init
        sourceMap += ref -> exp
        exp
    }
  }

  graph.controls.foreach { conf =>
    properties.put(conf.control, conf.properties)
  }

  def selfOption(implicit tx: S#Tx): Option[Obj[S]] = selfH.map(_.apply())

  def getProperty[A](c: Control, key: String): Option[A] = {
    val m0 = properties.get(c)
    if (m0 == null) None else {
      m0.get(key).asInstanceOf[Option[A]]
    }
  }
}

final class ContextImpl[S <: Sys[S]](protected val graph: Graph,
                                     protected val selfH: Option[stm.Source[S#Tx, Obj[S]]])
                                    (implicit val workspace: Workspace[S], val cursor: Cursor[S])
  extends ContextMixin[S]