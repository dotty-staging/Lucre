/*
 *  Cursor.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package stm

//object Cursor {
//  def inMemory[S <: Base[S]](base: S): Cursor[base.I] = ...
//}
trait Cursor[S <: Base[S]] {
  /** Issues a new transaction (executor), runs the function argument on it,
    * and returns the result.
    */
  def step[A](fun: S#Tx => A): A

  /** Issues a new transaction (executor), tagged with the given time
    * referring to "now", runs the function argument on it,
    * and returns the result.
    *
    * The tagging can be used for latency based circumstances, such as
    * scheduling OSC bundles on a sound server.
    *
    * @param  systemTimeNanos   Time in nano-seconds since midnight, January 1, 1970 UTC.
    *                           E.g. `System.currentTimeMillis() * 1000000000L` (possibly
    *                           adding nano-seconds fraction).
    */
  def stepTag[A](systemTimeNanos: Long)(fun: S#Tx => A): A

  def position(implicit tx: S#Tx): S#Acc
}