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

import de.sciss.lucre.stm.Sys

import scala.concurrent.stm.{Ref, TMap}

object ReactionMapImpl {
  private val noOpEval = () => ()
  private type AnyObsFun[S <: Sys[S]] = S#Tx => AnyRef => Unit

  def apply[S <: Sys[S]]: ReactionMap[S] = new Impl[S]

  private final case class EventObservation[S <: Sys[S]](fun: S#Tx => Any => Unit) {
    def reaction(parent: Event[S, Any], pull: Pull[S])(implicit tx: S#Tx): Reaction = () =>
      pull(parent) match {
        case Some(result) =>
          () => fun(tx)(result)
        case None => noOpEval
      }
  }

  private final class Impl[S <: Sys[S]] extends Mixin[S] {
    override def toString = s"ReactionMap@${hashCode.toHexString}"
  }

  trait Mixin[S <: Sys[S]] extends ReactionMap[S] {
    private val cnt       = Ref(0)
    private val eventMap  = TMap.empty[Int, EventObservation[S]]

    // self-reference useful when Mixin is added to an event.Sys
    def reactionMap: ReactionMap[S] = this

    final def processEvent(leaf: ObserverKey[S], parent: Event[S, Any], push: Push[S])
                          (implicit tx: S#Tx): Unit = {
      val itx = tx.peer
      eventMap.get(leaf.id)(itx).foreach { obs =>
        val react = obs.reaction(parent, push)
        push.addReaction(react)
      }
    }

    final def addEventReaction[A](fun: S#Tx => A => Unit)(implicit tx: S#Tx): ObserverKey[S] = {
      implicit val itx = tx.peer
      val key = cnt.get
      cnt.set(key + 1)
      eventMap.+=((key, new EventObservation[S](fun.asInstanceOf[S#Tx => Any => Unit])))(tx.peer)
      new ObserverKey[S](key)
    }

    def removeEventReaction(key: ObserverKey[S])(implicit tx: S#Tx): Unit =
      eventMap.-=(key.id)(tx.peer)
  }
}