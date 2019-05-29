/*
 *  IPush.scala
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

package de.sciss.lucre.event

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm.Base

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.collection.immutable.{Map => IMap}

object IPush {
  private[event] def apply[S <: Base[S], A](origin: IEvent[S, A], update: A)
                                           (implicit tx: S#Tx, targets: ITargets[S]): Unit = {
    val push = new Impl(origin, update)
    logEvent("ipush begin")
    push.visitChildren(origin)
    logEvent("ipull begin")
    push.pull()
    logEvent("ipull end")
  }

  type Parents[S <: Base[S]] = Set[IEvent[S, Any]]

  private def NoParents[S <: Base[S]]: Parents[S] = Set.empty[IEvent[S, Any]]

  // private type Visited[S <: Sys[S]] = IMap[Event[S, Any], Parents[S]]
  private final class Reaction[S <: Base[S], +A](update: A, observers: List[Observer[S, A]]) {
    def apply()(implicit tx: S#Tx): Unit =
      observers.foreach(_.apply(update))
  }

  private final class Impl[S <: Base[S]](origin: IEvent[S, Any], val update: Any)
                                        (implicit tx: S#Tx, targets: ITargets[S])
    extends IPull[S] {

    private[this] var pushMap   = IMap(origin -> NoParents[S])
    private[this] var pullMap   = IMap.empty[IEvent[S, Any], Option[Any]]

    private[this] var indent    = ""

    @elidable(CONFIG) private[this] def incIndent(): Unit = indent += "  "
    @elidable(CONFIG) private[this] def decIndent(): Unit = indent = indent.substring(2)

    private[this] def addVisited(child: IEvent[S, Any], parent: IEvent[S, Any]): Boolean = {
      val parents = pushMap.getOrElse(child, NoParents)
      logEvent(s"${indent}visit $child  (new ? ${parents.isEmpty})")
      pushMap += ((child, parents + parent))
      parents.isEmpty
    }

    def visitChildren(parent: IEvent[S, Any]): Unit = {
//      val inlet = parent.slot
      incIndent()
      try {
        val childEvents = targets.children(parent) // parent.node._targets.children
        childEvents.foreach { /* case (inlet2, */ child /* ) */ =>
//          if (inlet2 === inlet) {
            visit(child, parent)
//          }
        }

      } finally {
        decIndent()
      }
    }

    def visit(child: IEvent[S, Any], parent: IEvent[S, Any]): Unit =
      if (addVisited(child, parent)) visitChildren(child)

    def contains(source: IEvent[S, Any]): Boolean     = pushMap.contains(source)

    def isOrigin(that  : IEvent[S, Any]): Boolean     = that === origin
    def parents (child : IEvent[S, Any]): Parents[S]  = pushMap.getOrElse(child, NoParents)

    def pull(): Unit = {
      var reactCache  = List.empty[Reaction[S, Any]]
      var reactObs    = List.empty[Reaction[S, Any]]

      pushMap.foreach { case (event, _) =>
        val observers: List[Observer[S, Any]] = targets.getEventReactions(event)
        val isCache = event.isInstanceOf[Caching]

        if (observers.nonEmpty || isCache)
          apply[Any](event).foreach { v =>
            val r = new Reaction(v, observers)
            if (isCache) reactCache ::= r else reactObs ::= r
          }
      }

      logEvent(s"reactCache.size = ${reactCache.size}; reactObs.size = ${reactObs.size}")
      reactCache.reverse.foreach(_.apply())
      reactObs  .reverse.foreach(_.apply())
    }

    def resolve[A]: A = {
      logEvent(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    // caches pulled values
    def apply[A](source: IEvent[S, A]): Option[A] = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res) =>
            logEvent(s"${indent}pull $source  (new ? false)")
            res.asInstanceOf[Option[A]]
          case _ =>
            logEvent(s"${indent}pull $source  (new ? true)")
            val res = source.pullUpdate(this)
            pullMap += ((source, res))
            res
        }
      } finally {
        decIndent()
      }
    }
  }
}

trait IPull[S <: Base[S]] {
  /** Assuming that the caller is origin of the event, resolves the update of the given type. */
  def resolve[A]: A

  /** Retrieves the immediate parents from the push phase. */
  def parents(source: IEvent[S, Any]): IPush.Parents[S]

  /** Pulls the update from the given source. */
  def apply[A](source: IEvent[S, A]): Option[A]

  /** Whether the selector has been visited during the push phase. */
  def contains(source: IEvent[S, Any]): Boolean

  def isOrigin(source: IEvent[S, Any]): Boolean
}