/*
 *  NodeImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.impl

import de.sciss.lucre.event.{impl => evti}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

trait NodeImpl[S <: Sys[S], A]
  extends Expr[S, A]
  with evti.SingleNode[S, Change[A]] { self =>

  // final def changed: Event[S, Change[A]] = this

  // final def disposeData()(implicit tx: S#Tx) = ()

  override def toString = s"Expr$id"
}