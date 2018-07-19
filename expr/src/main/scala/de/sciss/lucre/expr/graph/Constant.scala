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
import de.sciss.lucre.stm.Base

final case class Constant[A](peer: A) extends Ex[A] {
  def value[S <: Base[S]](implicit tx: S#Tx): A = peer

  override def toString: String = peer.toString

  def aux: scala.List[Aux] = Nil
}