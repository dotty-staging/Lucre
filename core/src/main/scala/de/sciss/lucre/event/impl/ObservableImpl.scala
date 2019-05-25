/*
 *  ObservableImpl.scala
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

package de.sciss.lucre.event.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Disposable, Sys}

import scala.concurrent.stm.Ref

trait ObservableImpl[S <: Sys[S], U] extends Observable[S#Tx, U] {
  private[this] final class Observation(val fun: S#Tx => U => Unit) extends Disposable[S#Tx] {
    def dispose()(implicit tx: S#Tx): Unit = removeObservation(this)
  }

  private[this] val obsRef = Ref(Vector.empty[Observation])

  protected final def fire(update: U)(implicit tx: S#Tx): Unit = {
    val obs = obsRef.get(tx.peer)
    obs.foreach(_.fun(tx)(update))
  }

  private[this] def removeObservation(obs: Observation)(implicit tx: S#Tx): Unit =
    obsRef.transform(_.filterNot(_ === obs))(tx.peer)

  final def react(fun: S#Tx => U => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val obs = new Observation(fun)
    obsRef.transform(_ :+ obs)(tx.peer)
    obs
  }
}

trait DummyObservableImpl[S <: Sys[S]] extends Observable[S#Tx, Nothing] {
  def react(fun: S#Tx => Nothing => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = Disposable.empty
}