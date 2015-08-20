/*
 *  SingleNode.scala
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
package impl

import de.sciss.lucre.stm.Sys

/** Standalone events unite a node and one particular event.
  *
  * WARNING: the implementations of `equals` are really tricky right now. `EventImpl` is more specific in that
  * `VirtualNodeSelector` checks if the compared object is another `VirtualNodeSelector` whose reactor has the
  * same id and whose slot is the same. On the other hand `Invariant` inherits `equals` from `Reactor`
  * which checks for another reactor and then compares their ids.
  *
  * I don't know if `Reactor` still needs the `equals` implementation?
  */
trait SingleNode[S <: Sys[S], +A] extends Node[S] { self =>

  def changed: Event[S, A]

  final private[lucre] def event(slot: Int): Event[S, Any] = {
    if (slot != 0) throw new IllegalArgumentException(s"Invalid slot $slot")
    changed
  }

  trait Changed extends SingleEvent[S, A] {
    def node: Node[S] = self
  }
}