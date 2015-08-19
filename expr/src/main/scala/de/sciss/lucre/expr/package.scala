/*
 *  package.scala
 *  (Lucre)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import de.sciss.lucre.stm.Sys
import de.sciss.serial.{DataInput, DataOutput}

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

    IntExtensions    .init()
    BooleanExtensions.init()
  }

  private[this] object IntImpl extends impl.ExprTypeImpl[scala.Int] {
    final val typeID = 2

    def readValue (                  in : DataInput ): scala.Int  = in .readInt()
    def writeValue(value: scala.Int, out: DataOutput): Unit       = out.writeInt(value)
  }

  private[this] object LongImpl extends impl.ExprTypeImpl[scala.Long] {
    final val typeID = 3

    def readValue (                   in : DataInput ): scala.Long  = in .readLong()
    def writeValue(value: scala.Long, out: DataOutput): Unit        = out.writeLong(value)
  }

  private[this] object DoubleImpl extends impl.ExprTypeImpl[scala.Double] {
    final val typeID = 5

    def readValue (                     in : DataInput ): scala.Double  = in .readDouble()
    def writeValue(value: scala.Double, out: DataOutput): Unit          = out.writeDouble(value)
  }

  private[this] object BooleanImpl extends impl.ExprTypeImpl[scala.Boolean] {
    final val typeID = 6

    def readValue (                      in : DataInput ): scala.Boolean  = in .readBoolean()
    def writeValue(value: scala.Boolean, out: DataOutput): Unit           = out.writeBoolean(value)
  }

  private[this] object StringImpl extends impl.ExprTypeImpl[Predef.String] {
    final val typeID = 8

    def readValue (                      in : DataInput ): Predef.String  = in .readUTF()
    def writeValue(value: Predef.String, out: DataOutput): Unit           = out.writeUTF(value)
  }
}