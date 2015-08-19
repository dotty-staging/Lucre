/*
 *  SingleEvent.scala
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

trait SingleEvent[S <: Sys[S], +A] extends Event[S, A] {
  final private[event] def slot = 0
}
