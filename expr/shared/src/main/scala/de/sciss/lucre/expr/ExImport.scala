/*
 *  ExImport.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import java.net.{URI => _URI}

import de.sciss.lucre.Adjunct.{FromAny, HasDefault, ScalarOrd}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.language.implicitConversions

object ExImport extends ExImport
trait ExImport {
  implicit def stringLiteralExOps (x: String): StringLiteralExOps = new StringLiteralExOps(x)
  implicit def intLiteralExOps    (x: Int   ): IntLiteralExOps    = new IntLiteralExOps   (x)
  implicit def longLiteralExOps   (x: Long  ): LongLiteralExOps   = new LongLiteralExOps  (x)
  implicit def doubleLiteralExOps (x: Double): DoubleLiteralExOps = new DoubleLiteralExOps(x)

  def any2stringadd             : Any = () // disable this from scala.Predef
  def augmentString (x: String) : Any = () // disable this from scala.Predef
  def wrapString    (x: String) : Any = () // disable this from scala.Predef

  type Span     = _Span
  type SpanLike = _SpanLike

  implicit def spanLikeTop: FromAny[SpanLike] with HasDefault[SpanLike]                       = Ex.spanLikeTop
  implicit def spanTop    : FromAny[Span    ] with HasDefault[Span    ]                       = Ex.spanTop
  implicit def fileTop    : FromAny[_URI    ] with HasDefault[_URI] with ScalarOrd[_URI]      = Ex.fileTop
}