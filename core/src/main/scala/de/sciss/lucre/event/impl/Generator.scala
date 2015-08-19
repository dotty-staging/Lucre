package de.sciss.lucre.event
package impl

import de.sciss.lucre.stm.Sys

trait Generator[S <: Sys[S], A] extends Event[S, A] {
  final def fire(update: A)(implicit tx: S#Tx): Unit = {
    log(s"$this fire $update")
    Push(this, update)
  }
}