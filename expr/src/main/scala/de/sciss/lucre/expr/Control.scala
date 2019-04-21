/*
 *  Control.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys

import scala.language.higherKinds

object Control {
  final case class Configured(control: Control, properties: Map[String, Any]) {
    override def productPrefix: String = s"Control$$Configured"
  }
}
trait Control extends Product {
  type Repr[S <: Sys[S]] <: IControl[S]

  // this acts now as a fast unique reference
  @transient final private[this] lazy val ref = new AnyRef

  // ---- constructor ----
  Graph.builder.addControl(this)

  final def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S] =
    ctx.visit[Repr[S]](ref, mkControl)

  protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S]
}
