/*
 *  IEvent.scala
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

import de.sciss.lucre.stm.Base

/** In-memory (non-serializable) event. */
trait IEvent[S <: Base[S], +A] extends Observable[S#Tx, A] {
//  private[event] def slot: Int

  /** Connects the given selector to this event. That is, this event will
    * add the selector to its propagation targets.
    */
  def ---> (sink: IEvent[S, Any])(implicit tx: S#Tx): Unit

  /** Disconnects the given selector from this event. That is, this event will
    * remove the selector from its propagation targets.
    */
  def -/-> (sink: IEvent[S, Any])(implicit tx: S#Tx): Unit

  /** Involves this event in the pull-phase of event delivery. The event should check
    * the source of the originally fired event, and if it identifies itself with that
    * source, cast the `update` to the appropriate type `A` and wrap it in an instance
    * of `Some`. If this event is not the source, it should invoke `pull` on any
    * appropriate event source feeding this event.
    *
    * @return  the `update` as seen through this event, or `None` if the event did not
    *          originate from this part of the dependency graph or was absorbed by
    *          a filtering function
    */
  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[A]
}
