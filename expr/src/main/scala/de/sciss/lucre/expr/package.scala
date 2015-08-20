/*
 *  package.scala
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

package de.sciss.lucre

import de.sciss.lucre.stm.Sys
import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput}

package object expr {
  type Repr[A]      = {type L[~ <: Sys[~]] = Expr[~, A]}
  type TypeExpr1[A] = Type.Expr[A] with Type._1[Repr[A]#L]

  val Int    : TypeExpr1[scala.Int    ] = IntImpl
  val Long   : TypeExpr1[scala.Long   ] = LongImpl
  val Double : TypeExpr1[scala.Double ] = DoubleImpl
  val Boolean: TypeExpr1[scala.Boolean] = BooleanImpl
  val String : TypeExpr1[Predef.String] = StringImpl

  def init(): Unit = {
    Int    .init()
    Long   .init()
    Double .init()
    Boolean.init()
    String .init()
    List   .init()
    Map    .init()

    IntExtensions    .init()
    BooleanExtensions.init()
  }

  private[this] object IntImpl extends impl.ExprTypeImpl[scala.Int] {
    final val typeID = 2
    final val valueSerializer = ImmutableSerializer.Int
  }

  private[this] object LongImpl extends impl.ExprTypeImpl[scala.Long] {
    final val typeID = 3
    final val valueSerializer = ImmutableSerializer.Long
  }

  private[this] object DoubleImpl extends impl.ExprTypeImpl[scala.Double] {
    final val typeID = 5
    final val valueSerializer = ImmutableSerializer.Double
  }

  private[this] object BooleanImpl extends impl.ExprTypeImpl[scala.Boolean] {
    final val typeID = 6
    final val valueSerializer = ImmutableSerializer.Boolean
  }

  private[this] object StringImpl extends impl.ExprTypeImpl[Predef.String] {
    final val typeID = 8
    final val valueSerializer = ImmutableSerializer.String
  }
}