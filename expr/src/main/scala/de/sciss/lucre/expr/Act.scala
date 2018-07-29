/*
 *  Act.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.expr.Ex.Context
import de.sciss.lucre.stm.{Disposable, Sys}

object Act {
  final case class Link[A](source: Trig, sink: Act)
    extends Control {

    override def productPrefix = s"Act$$Link"

    type Repr[S <: Sys[S]] = Disposable[S#Tx]

    protected def mkControl[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val tr = source.expand[S]
      val ac = sink  .expand[S]
      tr.changed.react { implicit tx => _ => ac.execute() }
    }
  }
}
trait Act extends Product {
  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IAction[S]
}
