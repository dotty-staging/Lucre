/*
 *  Trig.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.{ITrigger, TrigOps}
import de.sciss.lucre.stm.Sys

import scala.language.{higherKinds, implicitConversions}

object Trig {
  final val Some: Option[Unit] = scala.Some(())

  implicit def ops(t: Trig): TrigOps = new TrigOps(t)
}

/** A trigger element.
  *
  * ''Important:'' Implementation inadvertently run into trouble if they do not
  * extend `Trig.Lazy` to avoid "splitting" of the event paths. The only reason this
  * is not enforced is that implementation may already mixin lazy traits such as
  * `Control` (and by extension, `Widget`).
  */
trait Trig extends Lazy {
  type Repr[S <: Sys[S]] <: ITrigger[S]
}