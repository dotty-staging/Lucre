/*
 *  Root.scala
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
package impl

import de.sciss.lucre.stm.Sys

/** A rooted event does not have sources. This trait provides a simple
  * implementation of `pull` which merely checks if this event has fired or not.
  */
trait Root[S <: Sys[S], +A] extends Event[S, A] {
  private[lucre] final def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = Some(pull.resolve[A])
}

/** A generator without further sources. */
trait RootGenerator[S <: Sys[S], A] extends Generator[S, A] with Root[S, A]