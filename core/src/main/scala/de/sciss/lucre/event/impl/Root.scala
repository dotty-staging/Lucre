/*
 *  Root.scala
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

/** A rooted event does not have sources. This trait provides a simple
  * implementation of `pull` which merely checks if this event has fired or not.
  */
trait Root[S <: Sys[S], +A] extends Event[S, A] {
  final private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = Some(pull.resolve[A])
}