/*
 *  Copy.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import de.sciss.lucre.stm.impl.CopyImpl

import scala.language.higherKinds
import scala.reflect.ClassTag

object Copy {
  def apply[In <: Sys[In], Out <: Sys[Out]](implicit txIn: In#Tx, txOut: Out#Tx): Copy[In, Out] =
    new CopyImpl[In, Out]

  def apply1[In <: Sys[In], Out <: Sys[Out]](implicit txIn: In#Tx, txOut: Out#Tx): Copy1[In, Out] =
    new CopyImpl[In, Out]
}
trait Copy[In <: Sys[In], Out <: Sys[Out]] {
  /** Makes a deep copy of the input element. Passing in an `Obj`
    * will also copy the attributes.
    */
  def apply[Repr[~ <: Sys[~]] <: Elem[~]](in: Repr[In]): Repr[Out]

  /** Provides a hint for the input element with key and value
    * by convention.
    */
  def putHint[A](in: Elem[In], key: String, value: A): Unit

  def getHint[A](in: Elem[In], key: String)(implicit ct: ClassTag[A]): Option[A]

  /** Copies all attributes from input to output. */
  def copyAttr(in: Obj[In], out: Obj[Out]): Unit

  /** Stores an early copy of an object from within its own `copy` method,
    * to allow mutual correspondences. The copy is only completed with
    * the provided `code` thunk argument which will be executed in the
    * `finish()` stage.
    */
  def defer[Repr[~ <: Sys[~]] <: Obj[~]](in: Repr[In], out: Repr[Out])(code: => Unit): Unit

  def finish(): Unit
}
/** Temporary interface extension until we can break binary compatibility. */
trait Copy1[In <: Sys[In], Out <: Sys[Out]] extends Copy[In, Out] {
  /** Copies the object but not its attributes. */
  def copyPlain[Repr[~ <: Sys[~]] <: Elem[~]](in: Repr[In]): Repr[Out]
}