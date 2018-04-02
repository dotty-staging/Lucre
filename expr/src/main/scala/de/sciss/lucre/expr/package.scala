/*
 *  package.scala
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

package de.sciss.lucre

import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.bitemp.{BiGroup, BiPin}
import de.sciss.lucre.event.Map
import de.sciss.lucre.stm.Sys

import scala.language.higherKinds

package object expr {
  // type ExprRepr[A] = {type L[~ <: Sys[~]] <: Expr[~, A]}

  /** An expression type with installable extensions. */
  type TypeExpr1[A, Repr[~ <: Sys[~]] <: expr.Expr[~, A]] = Type.Expr[A, Repr] with Type._1[Repr]

  // The `val` approach is nice because it hides
  // implementation details. Unfortunately that
  // doesn't give us a real companion object,
  // so implicit resolution, for example for
  // serializers, won't work...

  // val IntObj: TypeExpr1[Int, IntObj] = IntImpl

  def init(): Unit = {
    IntObj            .init()
    LongObj           .init()
    DoubleObj         .init()
    BooleanObj        .init()
    StringObj         .init()
    SpanLikeObj       .init()
    SpanObj           .init()
    IntVector         .init()
    DoubleVector      .init()

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
}