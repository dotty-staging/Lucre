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

import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.stm.Sys
import de.sciss.serial.ImmutableSerializer
import de.sciss.span

package object expr {
  type Repr[A]      = {type L[~ <: Sys[~]] = Expr[~, A]}
  type TypeExpr1[A] = Type.Expr[A] with Type._1[Repr[A]#L]

  val Int     : TypeExpr1[scala.Int     ] = IntImpl
  val Long    : TypeExpr1[scala.Long    ] = LongImpl
  val Double  : TypeExpr1[scala.Double  ] = DoubleImpl
  val Boolean : TypeExpr1[scala.Boolean ] = BooleanImpl
  val String  : TypeExpr1[Predef.String ] = StringImpl
  val SpanLike: TypeExpr1[span.SpanLike ] = SpanLikeImpl
  val Span    : TypeExpr1[span.Span     ] = SpanImpl

  def init(): Unit = {
    Int               .init()
    Long              .init()
    Double            .init()
    Boolean           .init()
    String            .init()
    SpanLike          .init()
    Span              .init()

    List              .init()
    Map               .init()
    Artifact          .init()
    ArtifactLocation  .init()

    IntExtensions     .init()
    LongExtensions    .init()
    DoubleExtensions  .init()
    BooleanExtensions .init()
    StringExtensions  .init()
    SpanLikeExtensions.init()
    SpanExtensions    .init()
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

  private[this] object SpanLikeImpl extends impl.ExprTypeImpl[span.SpanLike] {
    final val typeID = 9
    final val valueSerializer = span.SpanLike.serializer
  }

  private[this] object SpanImpl extends impl.ExprTypeImpl[span.Span] {
    final val typeID = 10
    final val valueSerializer = span.Span.serializer
  }
}