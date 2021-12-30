/*
 *  Cursor.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

object Cursor {
  def bridge[T <: Exec[T], I <: Exec[I]](peer: Cursor[T])(implicit b: T => I): Cursor[I] = new Bridge(peer)

  private final class Bridge[T <: Exec[T], I <: Exec[I]](peer: Cursor[T])(implicit b: T => I) extends Cursor[I] {
    def step[A](fun: I => A): A = peer.step[A] { tx => fun(b(tx)) }

    def stepTag[A](systemTimeNanos: Long)(fun: I => A): A = {
      peer.stepTag[A](systemTimeNanos) { tx =>
        fun(b(tx))
      }
    }
  }
}
trait Cursor[T <: Exec[T]] {
  /** Issues a new transaction (executor), runs the function argument on it,
   * and returns the result.
   */
  def step[A](fun: T => A): A

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
  def stepTag[A](systemTimeNanos: Long)(fun: T => A): A
}