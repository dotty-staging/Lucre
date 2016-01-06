/*
 *  MappingNode.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2016 Hanns Holger Rutz. All rights reserved.
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

/** A trait which combined external input events with self generated events. */
trait MappingNode[S <: Sys[S], A, B]
  extends Node[S] {

  protected def inputEvent: EventLike[S, B]

  /** Folds a new input event, by combining it with an optional previous output event. */
  protected def foldUpdate(generated: Option[A], input: B)(implicit tx: S#Tx): Option[A]

  trait Mapped extends Generator[S, A] {
    private[lucre] final def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = {
      val gen = if (pull.isOrigin(this)) Some(pull.resolve[A]) else None
      if (pull.contains(inputEvent)) pull(inputEvent) match {
        case Some(e)  => foldUpdate(gen, e)
        case _        => gen
      } else gen
    }
  }
}