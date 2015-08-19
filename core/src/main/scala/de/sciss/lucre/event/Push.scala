/*
 *  Push.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
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
import scala.collection.immutable.{IndexedSeq => Vec}

object Push {
  private[event] def apply[S <: Sys[S], A](origin: Event[S, A], update: A)(implicit tx: S#Tx): Unit = {
    val push = new Impl(origin, update)
    log("push begin")
    push.visitChildren(origin)
    log("pull begin")
    push.pull()
    log("pull end")
  }

  private val NoReactions = Vec.empty[Reaction]
  private val NoOpEval = () => ()

  type Parents[S <: Sys[S]] = Set[Event[S, Any]]

  private def NoParents [S <: Sys[S]]: Parents[S] = Set.empty[Event[S, Any]]

  private type Visited[S <: Sys[S]] = Map[Event[S, Any], Parents[S]]

  private final class Impl[S <: Sys[S]](origin: Event[S, Any], val update: Any)(implicit tx: S#Tx)
    extends Push[S] {
    private var pushMap   = Map((origin: Any, NoParents[S]))
    private var pullMap   = Map.empty[EventLike[S, Any], Option[Any]]
    private var reactions = NoReactions

    private var indent    = ""

    @elidable(CONFIG) private def incIndent(): Unit = indent += "  "
    @elidable(CONFIG) private def decIndent(): Unit = indent = indent.substring(2)

    private def addVisited(child: Event[S, Any], parent: Event[S, Any]): Boolean = {
      val parents = pushMap.getOrElse(child, NoParents)
      log(s"${indent}visit $child  (new ? ${parents.isEmpty})")
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
            child.pushUpdate(parent, this)
          }
        }
        val observers = tx.reactionMap.getEventReactions(parent)
        observers.foreach { observer =>
          val reaction: Reaction = () =>
            this(parent) match {
              case Some(result) =>
                () => observer(result)(tx)
              case None => NoOpEval
            }
          addReaction(reaction)
        }

      } finally {
        decIndent()
      }
    }

    def visit(child: Event[S, Any], parent: Event[S, Any]): Unit =
      if (addVisited(child, parent)) visitChildren(child)

    def contains(source: EventLike[S, Any]): Boolean    = pushMap.contains(source)
    def isOrigin(that  : EventLike[S, Any]): Boolean    = that == origin
    def parents (child : Event    [S, Any]): Parents[S] = pushMap.getOrElse(child, NoParents)

//    def addLeaf(leaf: ObserverKey[S], parent: Event[S, Any]): Unit = {
//      log(s"${indent}addLeaf $leaf, parent = $parent")
//      tx.reactionMap.processEvent(leaf, parent, this)
//    }

    def addReaction(r: Reaction): Unit = reactions :+= r

    def pull(): Unit = {
      log(s"numReactions = ${reactions.size}")
      val firstPass = reactions.map(_.apply())
      /* val secondPass = */ firstPass.foreach(_.apply())
    }

    def resolve[A]: A = {
      log(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    // caches pulled values
    def apply[A](source: EventLike[S, A]): Option[A] = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res: Option[_]) =>
            log(s"${indent}pull $source  (new ? false)")
            res.asInstanceOf[Option[A]]
          case _ =>
            log(s"${indent}pull $source  (new ? true)")
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

private[event] sealed trait Push[S <: Sys[S]] extends Pull[S] {

  private[event] def visit(sel: Event[S, Any], parent: Event[S, Any]): Unit

  // private[event] def addLeaf(leaf: ObserverKey[S], parent: Event[S, Any]): Unit

  private[event] def addReaction(r: Reaction): Unit
}