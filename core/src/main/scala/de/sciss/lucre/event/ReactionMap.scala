/*
 *  ReactionMap.scala
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

import de.sciss.lucre.stm.Sys

object ReactionMap {
  def apply[S <: Sys[S]](implicit tx: S#Tx): ReactionMap[S] = impl.ReactionMapImpl[S]
}

trait ReactionMap[S <: Sys[S]] {
  def addEventReaction   [A](event: Event[S, A  ], observer: Observer[S, A])(implicit tx: S#Tx): Boolean
  def removeEventReaction[A](event: Event[S, Any], observer: Observer[S, A])(implicit tx: S#Tx): Boolean

  def getEventReactions[A](event: Event[S, A])(implicit tx: S#Tx): List[Observer[S, A]]
  def hasEventReactions[A](event: Event[S, A])(implicit tx: S#Tx): Boolean
}