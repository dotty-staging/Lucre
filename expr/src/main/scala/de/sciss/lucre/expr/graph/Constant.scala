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
import de.sciss.lucre.event.{Dummy, Observable}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

object Constant {
  private final class Expanded[S <: Sys[S], A](peer: A)
    extends ExprLike[S, A] {

    override def toString: String = peer.toString

    def changed: Observable[S#Tx, Change[A]] = Dummy[S, Change[A]]

    def value(implicit tx: S#Tx): A = peer

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
final case class Constant[A](peer: A) extends Ex[A] {

  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): ExprLike[S, A] =
    new Constant.Expanded[S, A](peer)

  override def toString: String = peer.toString

  def aux: scala.List[Aux] = Nil
}