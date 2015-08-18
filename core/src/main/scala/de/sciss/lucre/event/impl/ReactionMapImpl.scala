/*
 *  ReactionMapImpl.scala
 *  (Lucre)
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
package impl

import de.sciss.lucre.stm.{IdentifierMap, Sys}

object ReactionMapImpl {
  private val noOpEval = () => ()
  private type AnyObsFun[S <: Sys[S]] = S#Tx => AnyRef => Unit

  def apply[S <: Sys[S]](implicit tx: S#Tx): ReactionMap[S] = new Impl[S](tx.newInMemoryIDMap)

  private final case class EventObservation[S <: Sys[S]](fun: S#Tx => Any => Unit) {
    def reaction(parent: Event[S, Any], pull: Pull[S])(implicit tx: S#Tx): Reaction = () =>
      pull(parent) match {
        case Some(result) =>
          () => fun(tx)(result)
        case None => noOpEval
      }
  }

  private final class Impl[S <: Sys[S]](protected val eventMap: IdentifierMap[S#ID, S#Tx, Map[Int, List[Observer[S]]]])
    extends Mixin[S] {

    override def toString = s"ReactionMap@${hashCode.toHexString}"
  }

  trait Mixin[S <: Sys[S]] extends ReactionMap[S] {
    protected def eventMap: IdentifierMap[S#ID, S#Tx, Map[Int, List[Observer[S]]]]

    // self-reference useful when Mixin is added to an event.Sys
    def reactionMap: ReactionMap[S] = this

//    final def processEvent(leaf: ObserverKey[S], parent: Event[S, Any], push: Push[S])
//                          (implicit tx: S#Tx): Unit = {
//      val itx = tx.peer
//      eventMap.get(leaf.id)(itx).foreach { obs =>
//        val react = obs.reaction(parent, push)
//        push.addReaction(react)
//      }
//    }

    final def addEventReaction[A](event: Event[S, Any], observer: Observer[S])(implicit tx: S#Tx): Boolean = {
      val id    = event.node.id
      val slot  = event.slot
      val map0  = eventMap.getOrElse(id  , Map.empty)
      val list0 = map0    .getOrElse(slot, Nil)
      val list1 = list0 ::: observer :: Nil
      val map1  = map0 + (slot -> list1)
      eventMap.put(id, map1)
      list0.isEmpty
    }

    def removeEventReaction(event: Event[S, Any], observer: Observer[S])(implicit tx: S#Tx): Boolean = {
      val id    = event.node.id
      val slot  = event.slot
      val map0  = eventMap.getOrElse(id  , throw new NoSuchElementException(s"Node $id"))
      val list0 = map0    .getOrElse(slot, throw new NoSuchElementException(s"slot $slot"))
      val list1 = list0.filterNot(_ == observer)
      val map1  = if (list1.isEmpty) map0 - slot else map0 + (slot -> list1)
      if (map1.isEmpty) {
        eventMap.remove(id)
      } else {
        eventMap.put(id, map1)
      }
      list1.isEmpty
    }

    def hasEventReactions(event: Event[S, Any])(implicit tx: S#Tx): Boolean =
      eventMap.get(event.node.id).exists(_.get(event.slot).isDefined)
  }
}