/*
 *  Const.scala
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

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.event.{IChangeEvent, IDummy}
import de.sciss.lucre.stm.{Base, Sys}

object Const {
  private[sciss] final class Expanded[S <: Base[S], A](peer: A)
    extends IExpr[S, A] {

    override def toString: String = peer.toString

    def changed: IChangeEvent[S, A] = IDummy.applyChange

    def value(implicit tx: S#Tx): A = peer

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
final case class Const[A](value: A) extends Ex[A] {
  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
    new Const.Expanded[S, A](value)

  override def toString: String = value.toString
}