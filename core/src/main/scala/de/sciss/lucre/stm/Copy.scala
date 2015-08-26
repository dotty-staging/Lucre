/*
 *  Copy.scala
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

package de.sciss.lucre.stm

import de.sciss.lucre.stm.impl.CopyImpl

import scala.language.implicitConversions

object Copy {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Copy[S] = new CopyImpl[S]
}
trait Copy[S <: Sys[S]] {
  /** Makes a deep copy of the input element. Passing in an `Obj`
    * will also copy the attributes.
    */
  def apply[Repr <: Elem[S]](in: Repr): Repr

  /** Provides a hint for the input element with key and value
    * by convention.
    */
  def putHint[Repr <: Elem[S]](in: Repr, key: String, value: Any): Unit

  def getHint[Repr <: Elem[S]](in: Repr, key: String): Option[Any]

  /** Stores an early copy of an object from within its own `copy` method,
    * to allow mutual correspondences.
    */
  def provide[Repr <: Obj[S]](in: Repr, out: Repr): Unit
}
