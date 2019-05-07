/*
 *  TrigOps.scala
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

import de.sciss.lucre.expr.graph.{Act, Ex, TBinaryOp, TTBinaryOp, Trig}

import scala.language.implicitConversions

object TrigOps {
  implicit def trigOps(t: Trig): TrigOps = new TrigOps(t)
}
final class TrigOps(private val t: Trig) extends AnyVal {
  def & (that: Trig): Trig = TTBinaryOp(TTBinaryOp.And(), t, that)
  def | (that: Trig): Trig = TTBinaryOp(TTBinaryOp.Or (), t, that)
  def ^ (that: Trig): Trig = TTBinaryOp(TTBinaryOp.Xor(), t, that)

  def filter(that: Ex[Boolean]): Trig = TBinaryOp(TBinaryOp.And(), t, that)

//  def filterNot (ex: Ex[Boolean]): Trig = {
//    import ExOps._
//    filter(!ex)
//  }

  def ---> [A <: Act](act: A): A = {
    Act.Link(t, act)
    act
  }
}