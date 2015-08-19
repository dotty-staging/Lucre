/*
 *  Push.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event

import de.sciss.lucre.stm.Sys

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.collection.breakOut

object Push {
  private[event] def apply[S <: Sys[S], A](origin: Event[S, A], update: A)(implicit tx: S#Tx): Unit = {
    val push = new Impl(origin, update)
    logEvent("push begin")
    push.visitChildren(origin)
    logEvent("pull begin")
    push.pull()
    logEvent("pull end")
  }

  type Parents[S <: Sys[S]] = Set[Event[S, Any]]

  private def NoParents[S <: Sys[S]]: Parents[S] = Set.empty[Event[S, Any]]

  // private type Visited[S <: Sys[S]] = Map[Event[S, Any], Parents[S]]
  private final class Reaction[S <: Sys[S], +A](update: A, observers: List[Observer[S, A]]) {
    def apply()(implicit tx: S#Tx): Unit =
      observers.foreach(_.apply(update))
  }

  private final class Impl[S <: Sys[S]](origin: Event[S, Any], val update: Any)(implicit tx: S#Tx)
    extends Pull[S] {
    private var pushMap   = Map(origin -> NoParents[S])
    private var pullMap   = Map.empty[EventLike[S, Any], Option[Any]]

    private var indent    = ""

    @elidable(CONFIG) private def incIndent(): Unit = indent += "  "
    @elidable(CONFIG) private def decIndent(): Unit = indent = indent.substring(2)

    private def addVisited(child: Event[S, Any], parent: Event[S, Any]): Boolean = {
      val parents = pushMap.getOrElse(child, NoParents)
      logEvent(s"${indent}visit $child  (new ? ${parents.isEmpty})")
      pushMap += ((child, parents + parent))
      parents.isEmpty
    }

    def visitChildren(parent: Event[S, Any]): Unit = {
      val inlet = parent.slot
      incIndent()
      try {
        val childEvents = parent.node._targets.children
        childEvents.foreach { case (inlet2, child) =>
          if (inlet2 == inlet) {
            visit(child, parent)
          }
        }

      } finally {
        decIndent()
      }
    }

    def visit(child: Event[S, Any], parent: Event[S, Any]): Unit =
      if (addVisited(child, parent)) visitChildren(child)

    // cf. https://stackoverflow.com/questions/32098481
    def contains(source: EventLike[S, Any]): Boolean = source match {
      case e: Event[S, Any] => pushMap.contains(e)
      case _ => false
    }

    def isOrigin(that  : EventLike[S, Any]): Boolean    = that == origin
    def parents (child : Event    [S, Any]): Parents[S] = pushMap.getOrElse(child, NoParents)

    def pull(): Unit = {
      val reactions: List[Reaction[S, Any]] = pushMap.flatMap { case (event, _) =>
        val observers = tx.reactionMap.getEventReactions(event)
        if (observers.isEmpty) None else apply[Any](event).map(new Reaction(_, observers))
      } (breakOut)
      logEvent(s"numReactions = ${reactions.size}")
      reactions.foreach(_.apply())
    }

    def resolve[A]: A = {
      logEvent(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    // caches pulled values
    def apply[A](source: EventLike[S, A]): Option[A] = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res: Option[_]) =>
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

sealed trait Pull[S <: Sys[S]] {
  /** Assuming that the caller is origin of the event, resolves the update of the given type. */
  def resolve[A]: A

  /** Retrieves the immediate parents from the push phase. */
  def parents(source: Event[S, Any]): Push.Parents[S]

  /** Pulls the update from the given source. */
  def apply[A](source: EventLike[S, A]): Option[A]

  /** Whether the selector has been visited during the push phase. */
  def contains(source: EventLike[S, Any]): Boolean

  def isOrigin(source: EventLike[S, Any]): Boolean
}