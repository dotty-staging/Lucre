/*
 *  TrigOps.scala
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

import scala.language.implicitConversions

import de.sciss.lucre.expr.graph.{TTBinaryOp => BinOp}

object TrigOps {
  implicit def trigOps(t: Trig): TrigOps = new TrigOps(t)
}
final class TrigOps(private val t: Trig) extends AnyVal {
  def & (that: Trig): Trig = BinOp(BinOp.And(), t, that)
  def | (that: Trig): Trig = BinOp(BinOp.Or (), t, that)
  def ^ (that: Trig): Trig = BinOp(BinOp.Xor(), t, that)

//  def filter    (ex: Ex[Boolean]): Trig = ...
//  def filterNot (ex: Ex[Boolean]): Trig = ...
}