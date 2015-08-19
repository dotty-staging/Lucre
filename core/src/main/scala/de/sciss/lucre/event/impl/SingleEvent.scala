package de.sciss.lucre.event
package impl

import de.sciss.lucre.stm.Sys

trait SingleEvent[S <: Sys[S], +A] extends Event[S, A] {
  final private[event] def slot = 0
}
