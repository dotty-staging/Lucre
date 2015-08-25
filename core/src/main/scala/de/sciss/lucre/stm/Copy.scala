package de.sciss.lucre.stm

import scala.language.implicitConversions

trait Copy[S <: Sys[S]] {
  def apply[Repr <: Elem[S]](in: Repr): Repr

  /** Provide an early copy of an object from within its own `copy` method,
    * to allow mutual correspondences.
    */
  def provide[Repr <: Obj[S]](in: Repr, out: Repr): Unit
}
