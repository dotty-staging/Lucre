/*
 *  EventImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event.impl

import de.sciss.lucre.event.{Event, Observer}
import de.sciss.lucre.stm.{Disposable, Sys}

trait EventImpl[S <: Sys[S], +A] extends Event[S, A] {
  // ---- implemented ----

  final def ---> (sink: Event[S, Any])(implicit tx: S#Tx): Unit =
    node._targets.add(slot, sink)

  final def -/-> (sink: Event[S, Any])(implicit tx: S#Tx): Unit =
    node._targets.remove(slot, sink)

  final def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = Observer(this, fun)
}