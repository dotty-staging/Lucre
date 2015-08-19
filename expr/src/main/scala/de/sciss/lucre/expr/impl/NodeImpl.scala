/*
 *  NodeImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.event.{impl => evti}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

trait NodeImpl[S <: Sys[S], A]
  extends Expr.Node[S, A]
  with evti.SingleNode[S, Change[A]] { self =>

  // final def changed: Event[S, Change[A]] = this

  // final def disposeData()(implicit tx: S#Tx) = ()

  trait Changed extends evti.SingleEvent[S, Change[A]] {
    def node: Expr.Node[S, A] = self
  }

  override def toString = s"Expr$id"
}