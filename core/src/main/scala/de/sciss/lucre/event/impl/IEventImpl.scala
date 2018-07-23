/*
 *  IEventImpl.scala
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

package de.sciss.lucre.event
package impl

import de.sciss.lucre.stm.{Base, Disposable}

trait IEventImpl[S <: Base[S], +A] extends IEvent[S, A] {
  protected def targets: ITargets[S]

  def --->(sink: IEvent[S, Any])(implicit tx: S#Tx): Unit =
    targets.add(this, sink)

  def -/->(sink: IEvent[S, Any])(implicit tx: S#Tx): Unit =
    targets.remove(this, sink)

  def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
    Observer(this, fun)(tx, targets)
}