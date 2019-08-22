/*
 *  ExImport.scala
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

import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.language.implicitConversions

object ExImport {
  implicit def stringLiteralExOps(x: String): StringLiteralExOps = new StringLiteralExOps(x)

  def any2stringadd: Any = () // yeah, fuck you too

  type Span     = _Span
  type SpanLike = _SpanLike
}