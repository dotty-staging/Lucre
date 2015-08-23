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
import de.sciss.lucre.bitemp.{BiPin, BiGroup}
import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.Sys
import de.sciss.serial.ImmutableSerializer
import de.sciss.span.{Span, SpanLike}

import scala.language.higherKinds

package object expr {
  // type ExprRepr[A] = {type L[~ <: Sys[~]] <: Expr[~, A]}

  /** An expression type with installable extensions. */
  type TypeExpr1[A, Repr[~ <: Sys[~]] <: expr.Expr[~, A]] = Type.Expr[A, Repr] with Type._1[Repr]

  trait IntObj[S <: Sys[S]] extends Expr[S, Int]
  // The `val` approach is nice because it hides
  // implementation details. Unfortunately that
  // doesn't give us a real companion object,
  // so implicit resolution, for example for
  // serializers, won't work...

  // val IntObj: TypeExpr1[Int, IntObj] = IntImpl

  trait LongObj[S <: Sys[S]] extends Expr[S, Long]
  // val LongObj: TypeExpr1[Long, LongObj] = LongImpl

  trait DoubleObj[S <: Sys[S]] extends Expr[S, Double]
  // val DoubleObj: TypeExpr1[Double, DoubleObj] = DoubleImpl

  trait BooleanObj[S <: Sys[S]] extends Expr[S, Boolean]
  // val BooleanObj: TypeExpr1[Boolean, BooleanObj] = BooleanImpl

  trait StringObj[S <: Sys[S]] extends Expr[S, String]
  // val StringObj: TypeExpr1[String, StringObj] = StringImpl

  trait SpanLikeObj[S <: Sys[S]] extends Expr[S, SpanLike]
  // val SpanLikeObj: TypeExpr1[SpanLike, SpanLikeObj] = SpanLikeImpl

  trait SpanObj[S <: Sys[S]] extends Expr[S, Span]
  // val SpanObj: TypeExpr1[Span, SpanObj] = SpanImpl

  def init(): Unit = {
    IntObj            .init()
    LongObj           .init()
    DoubleObj         .init()
    BooleanObj        .init()
    StringObj         .init()
    SpanLikeObj       .init()
    SpanObj           .init()

    List              .init()
    Map               .init()
    Artifact          .init()
    ArtifactLocation  .init()

    BiPin             .init()
    BiGroup           .init()

    IntExtensions     .init()
    LongExtensions    .init()
    DoubleExtensions  .init()
    BooleanExtensions .init()
    StringExtensions  .init()
    SpanLikeExtensions.init()
    SpanExtensions    .init()
  }

  object IntObj extends impl.ExprTypeImpl[Int, IntObj] {
    final val typeID = 2
    final val valueSerializer = ImmutableSerializer.Int

    protected def mkConst[S <: Sys[S]](id: S#ID, value: Int)(implicit tx: S#Tx): Const[S] = ???

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]])(implicit tx: S#Tx): Var[S] = ???
  }

  object LongObj extends impl.ExprTypeImpl[Long, LongObj] {
    final val typeID = 3
    final val valueSerializer = ImmutableSerializer.Long

    protected def mkConst[S <: Sys[S]](id: S#ID, value: Long)(implicit tx: S#Tx): Const[S] = ???

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]])(implicit tx: S#Tx): Var[S] = ???
  }

  object DoubleObj extends impl.ExprTypeImpl[Double, DoubleObj] {
    final val typeID = 5
    final val valueSerializer = ImmutableSerializer.Double

    protected def mkConst[S <: Sys[S]](id: S#ID, value: Double)(implicit tx: S#Tx): Const[S] = ???

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]])(implicit tx: S#Tx): Var[S] = ???
  }

  object BooleanObj extends impl.ExprTypeImpl[Boolean, BooleanObj] {
    final val typeID = 6
    final val valueSerializer = ImmutableSerializer.Boolean

    protected def mkConst[S <: Sys[S]](id: S#ID, value: Boolean)(implicit tx: S#Tx): Const[S] = ???

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]])(implicit tx: S#Tx): Var[S] = ???
  }

  object StringObj extends impl.ExprTypeImpl[String, StringObj] {
    final val typeID = 8
    final val valueSerializer = ImmutableSerializer.String

    protected def mkConst[S <: Sys[S]](id: S#ID, value: String)(implicit tx: S#Tx): Const[S] = ???

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]])(implicit tx: S#Tx): Var[S] = ???
  }

  object SpanLikeObj extends impl.ExprTypeImpl[SpanLike, SpanLikeObj] {
    final val typeID = 9
    final val valueSerializer = SpanLike.serializer

    protected def mkConst[S <: Sys[S]](id: S#ID, value: SpanLike)(implicit tx: S#Tx): Const[S] = ???

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]])(implicit tx: S#Tx): Var[S] = ???
  }

  object SpanObj extends impl.ExprTypeImpl[Span, SpanObj] {
    final val typeID = 10
    final val valueSerializer = Span.serializer

    protected def mkConst[S <: Sys[S]](id: S#ID, value: Span)(implicit tx: S#Tx): Const[S] = ???

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]])(implicit tx: S#Tx): Var[S] = ???
  }
}