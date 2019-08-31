/*
 *  ITargets.scala
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

import de.sciss.lucre.stm.{Base, Sys, TxnLike}

import scala.annotation.tailrec
import scala.concurrent.stm.TMap

object ITargets {
  def apply[S <: Sys[S]]: ITargets[S] = new Impl[S]

  private final class Impl[S <: Sys[S]] extends ITargets[S] {
    import TxnLike.peer

    private[this] val connections = TMap.empty[IEvent[S, Any], List[IEvent[S, Any]]]
    private[this] val reactions   = TMap.empty[IEvent[S, Any], List[Observer[S, _]]]

    def children(parent: IEvent[S, Any])(implicit tx: S#Tx): List[IEvent[S, Any]] =
      connections.get(parent).getOrElse(Nil)

    def add(parent: IEvent[S, Any], sel: IEvent[S, Any])(implicit tx: S#Tx): Boolean = {
      val before    = children(parent)
      val wasEmpty  = before.isEmpty
      val now       = if (wasEmpty) sel :: Nil else before :+ sel
      connections  += parent -> now
      wasEmpty
    }

    @tailrec
    private def listRemove[A](item: A, rem: List[A], res: List[A]): List[A] = rem match {
      case `item` :: tail => res.reverse ::: tail
      case Nil            => res.reverse
      case head :: tail   => listRemove(item, tail, head :: res)
    }

    def remove(parent: IEvent[S, Any], sel: IEvent[S, Any])(implicit tx: S#Tx): Boolean = {
      val before    = children(parent)
      val now       = listRemove(sel, before, Nil)
      val isEmpty   = now.isEmpty

      if (isEmpty)
        connections -= parent
      else
        connections += parent -> now

      isEmpty
    }

    def addEventReaction[A](event: IEvent[S, A], observer: Observer[S, A])(implicit tx: S#Tx): Boolean = {
      val before    = getEventReactions(event)
      val wasEmpty  = before.isEmpty
      val now       = if (wasEmpty) observer :: Nil else before :+ observer
      reactions    += event -> now
      wasEmpty
    }

    def removeEventReaction[A](event: IEvent[S, Any], observer: Observer[S, A])(implicit tx: S#Tx): Boolean = {
      val before    = getEventReactions(event)
      val now       = listRemove(observer, before, Nil)
      val isEmpty   = now.isEmpty

      if (isEmpty)
        reactions += event -> now
      else
        reactions -= event

      isEmpty
    }

    def getEventReactions[A](event: IEvent[S, A])(implicit tx: S#Tx): List[Observer[S, A]] =
      reactions.get(event) match {
        case Some(xs) => xs.asInstanceOf[List[Observer[S, A]]]
        case None     => Nil
      }

    def hasEventReactions[A](event: IEvent[S, A])(implicit tx: S#Tx): Boolean =
      reactions.contains(event)
  }
}

/** Interconnection management for in-memory events.
  * A centralized instance that combines the functionality of `Targets` with `ReactionMap`.
  */
trait ITargets[S <: Base[S]] {
  def children(parent: IEvent[S, Any])(implicit tx: S#Tx): List[IEvent[S, Any]]

  /** Adds a dependant to this node target.
    *
    * @param parent the parent event to be pushing to the dependant
    * @param sel  the target selector to which an event will be pushed
    *
    * @return  `true` if this was the first dependant registered with the given parent, `false` otherwise
    */
  def add(parent: IEvent[S, Any], sel: IEvent[S, Any])(implicit tx: S#Tx): Boolean

  /** Removes a dependant from this node target.
    *
    * @param parent the parent event which is currently pushing to the dependant
    * @param sel  the target selector which was registered with the parent
    *
    * @return  `true` if this was the last dependant unregistered with the given parent, `false` otherwise
    */
  def remove(parent: IEvent[S, Any], sel: IEvent[S, Any])(implicit tx: S#Tx): Boolean

  // ---- reaction map ----

  def addEventReaction    [A](event: IEvent[S, A  ], observer: Observer[S, A])(implicit tx: S#Tx): Boolean
  def removeEventReaction [A](event: IEvent[S, Any], observer: Observer[S, A])(implicit tx: S#Tx): Boolean
  def getEventReactions   [A](event: IEvent[S, A])(implicit tx: S#Tx): List[Observer[S, A]]
  def hasEventReactions   [A](event: IEvent[S, A])(implicit tx: S#Tx): Boolean
}
