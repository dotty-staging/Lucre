/*
 *  Constant.scala
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
package graph

import de.sciss.lucre.aux.Aux
import de.sciss.lucre.event.{IDummy, IEvent}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

object Constant {
  private final class Expanded[S <: Base[S], A](peer: A)
    extends IExpr[S, A] {

    override def toString: String = peer.toString

    def changed: IEvent[S, Change[A]] = IDummy.apply

    def value(implicit tx: S#Tx): A = peer

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
final case class Constant[A](value: A) extends Ex[A] {

  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] =
    new Constant.Expanded[S, A](value)

  override def toString: String = value.toString

  def aux: scala.List[Aux] = Nil
}