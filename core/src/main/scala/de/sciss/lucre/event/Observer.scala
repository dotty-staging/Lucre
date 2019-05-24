/*
 *  Observer.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event

import de.sciss.lucre.stm.{Base, Disposable, Sys}

object Observer {
  def apply[S <: Sys[S], A](event: Event[S, A], fun: S#Tx => A => Unit)
                           (implicit tx: S#Tx): Observer[S, A] = {
    new Impl(event, tx, fun)
  }

  def apply[S <: Base[S], A](event: IEvent[S, A], fun: S#Tx => A => Unit)
                           (implicit tx: S#Tx, targets: ITargets[S]): Observer[S, A] = {
    new IImpl(event, tx, fun)
  }

  private final class IImpl[S <: Base[S], A](event: IEvent[S, A], tx0: S#Tx, fun: S#Tx => A => Unit)
                                            (implicit targets: ITargets[S])
    extends Observer[S, A] {

    override def toString = s"Observer<$event>"

    targets.addEventReaction[A](event, this)(tx0)

    def apply(update: A)(implicit tx: S#Tx): Unit = fun(tx)(update)

    def dispose()(implicit tx: S#Tx): Unit = {
      targets.removeEventReaction(event, this)
    }
  }
  private final class Impl[S <: Sys[S], A](event0: Event[S, A], tx0: S#Tx, fun: S#Tx => A => Unit)
    extends Observer[S, A] {

    override def toString = s"Observer<${event0.node.id}, ${event0.slot}>"

    private[this] val eventH = tx0.newHandle(event0: Event[S, Any])

    tx0.reactionMap.addEventReaction[A](event0, this)(tx0)

    def event(implicit tx: S#Tx): Event[S, Any] = eventH()

    def apply(update: A)(implicit tx: S#Tx): Unit = fun(tx)(update)

    def dispose()(implicit tx: S#Tx): Unit = {
      val event = this.event
      tx.reactionMap.removeEventReaction(event, this)
    }
  }

  /** This method is cheap. */
  def dummy[S <: Base[S]]: Disposable[S#Tx] = Dummy

  private object Dummy extends Disposable[Any] {
    override def toString = "Observer.Dummy"

    def dispose()(implicit tx: Any): Unit = ()
  }
}
trait Observer[S <: Base[S], -A] extends Disposable[S#Tx] {
//  def event(implicit tx: S#Tx): Event[S, Any]
  def apply(update: A)(implicit tx: S#Tx): Unit
}