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
  def apply[S <: Sys[S]](): ReactionMap[S] = impl.ReactionMapImpl[S]
}

trait ReactionMap[S <: Sys[S]] {
  def addEventReaction[A](fun: S#Tx => A => Unit)(implicit tx: S#Tx): ObserverKey[S]

  def removeEventReaction(key: ObserverKey[S])(implicit tx: S#Tx): Unit

  def processEvent(leaf: ObserverKey[S], parent: Event[S, Any], push: Push[S])(implicit tx: S#Tx): Unit
}