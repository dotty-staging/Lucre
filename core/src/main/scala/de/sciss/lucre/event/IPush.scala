/*
 *  IPush.scala
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

package de.sciss.lucre.event

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm.Base
import de.sciss.model.Change

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
      val m0    = currentPull.get()
      val isNew = !m0.contains(tx)
      if (isNew) currentPull.set(m0 + (tx -> this))
      try {
        val reactions: List[Reaction[S, Any]] = pushMap.iterator.flatMap { case (event, _) =>
          val observers: List[Observer[S, Any]] = targets.getEventReactions(event) // tx.reactionMap.getEventReactions(event)
          val hasObs = observers.nonEmpty
          if (hasObs || event.isInstanceOf[Caching]) {
            val opt = apply[Any](event)
            if (hasObs) opt.map(new Reaction(_, observers)) else None
          } else None
        } .toList
        logEvent(s"numReactions = ${reactions.size}")
        reactions.foreach(_.apply())
      } finally {
        if (isNew) currentPull.set(m0)
      }
    }

    def resolve[A]: A = {
      logEvent(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    def resolveChange[A](isNow: Boolean): A = {
      logEvent(s"${indent}resolveChange")
      val ch = update.asInstanceOf[Change[A]]
      if (isNow) ch.now else ch.before
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

    // caches pulled values
    def applyChange[A](source: IChangeEvent[S, A], isNow: Boolean): A = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res) if res.isDefined =>
            logEvent(s"${indent}pull $source  (new ? false)")
            val opt = res.asInstanceOf[Option[A]]
            opt.get
          case _ =>
            logEvent(s"${indent}pull $source  (new ? true)")
            val res = source.pullChange(this, isNow = isNow)
            pullMap += ((source, Some(res)))
            res
        }
      } finally {
        decIndent()
      }
    }
  }

  private val currentPull = new ThreadLocal[IMap[Any, IPull[_]]]() {
    override def initialValue(): IMap[Any, IPull[_]] = IMap.empty
  }

  def tryPull[S <: Base[S], A](source: IEvent[S, A])(implicit tx: S#Tx): Option[A] =
    currentPull.get().get(tx).flatMap { p =>
      val pc = p.asInstanceOf[IPull[S]]
      if (pc.contains(source)) pc(source) else None
    }
}

trait IPull[S <: Base[S]] {
  /** Assuming that the caller is origin of the event, resolves the update of the given type. */
  def resolve[A]: A

  def resolveChange[A](isNow: Boolean): A

  /** Retrieves the immediate parents from the push phase. */
  def parents(source: IEvent[S, Any]): IPush.Parents[S]

  /** Pulls the update from the given source. */
  def apply[A](source: IEvent[S, A]): Option[A]

  def applyChange[A](source: IChangeEvent[S, A], isNow: Boolean): A

  /** Whether the selector has been visited during the push phase. */
  def contains(source: IEvent[S, Any]): Boolean

  def isOrigin(source: IEvent[S, Any]): Boolean
}