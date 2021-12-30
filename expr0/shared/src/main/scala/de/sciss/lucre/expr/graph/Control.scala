/*
 *  Control.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Txn
import de.sciss.lucre.expr.{Graph, IControl}

object Control {
  final case class Configured(control: Control, properties: Map[String, Any]) {
    override def productPrefix: String = s"Control$$Configured"
  }
}
trait Control extends Lazy {
  type Repr[T <: Txn[T]] <: IControl[T]

  final def token: AnyRef = ref

  // ---- constructor ----
  Graph.builder.addControl(this)
}
