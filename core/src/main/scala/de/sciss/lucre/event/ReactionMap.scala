/*
 *  ReactionMap.scala
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

import de.sciss.lucre.stm.Sys

object ReactionMap {
  def apply[S <: Sys[S]](implicit tx: S#Tx): ReactionMap[S] = impl.ReactionMapImpl[S]
}

trait ReactionMap[S <: Sys[S]] {
  def addEventReaction[A](event: Event[S, Any], observer: Observer[S])(implicit tx: S#Tx): Boolean
  def removeEventReaction(event: Event[S, Any], observer: Observer[S])(implicit tx: S#Tx): Boolean

  // def processEvent(leaf: ObserverKey[S], parent: Event[S, Any], push: Push[S])(implicit tx: S#Tx): Unit

  def getEventReactions(event: Event[S, Any])(implicit tx: S#Tx): List[Observer[S]]
  def hasEventReactions(event: Event[S, Any])(implicit tx: S#Tx): Boolean
}