/*
 *  Observable.scala
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

object Observable {
  def empty[Tx, A]: Observable[Tx, A] = Empty.asInstanceOf[Observable[Tx, A]]

  private object Empty extends Observable[Any, Nothing] {
    override def toString = "Observable.empty"

    def react(fun: Any => Nothing => Unit)(implicit tx: Any): Disposable[Any] = Disposable.empty
  }
}
trait Observable[Tx, +A] {
  /** Registers a live observer with this observable. The method is called with the
   * observing function which receives the observable's update message of type `A`, and the
   * method generates an opaque `Disposable` instance, which may be used to
   * remove the observer eventually (through the `dispose` method).
   */
  def react(fun: Tx => A => Unit)(implicit tx: Tx): Disposable[Tx]
}