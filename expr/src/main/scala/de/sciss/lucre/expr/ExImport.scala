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

import java.io.File

import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.expr.graph.{BinaryOp => BinOp, UnaryOp => UnOp}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.language.implicitConversions

object ExImport {
  implicit def stringToExAttr(x: String): StringToExAttr = new StringToExAttr(x)

  def any2stringadd: Any = () // yeah, fuck you too

  type Span     = _Span
  type SpanLike = _SpanLike

  implicit final class fileOps(private val x: Ex[File]) extends AnyVal {
    /** Returns the parent directory if it exists. */
    def parentOption: Ex[Option[File]] =
      UnOp(UnOp.FileParentOption(), x)

    /** Returns the string representation of the file's path. */
    def path: Ex[String] =
      UnOp(UnOp.FilePath(), x)

    /** Returns the name part of the file. */
    def name: Ex[String] =
      UnOp(UnOp.FileName(), x)

    /** Returns the name part of the file and drops the extension (if any). */
    def base: Ex[String] =
      UnOp(UnOp.FileBase(), x)

    /** Returns the extension of the file (lower-cased, period dropped). Returns and empty string
      * if no extension is given.
      */
    def ext: Ex[String] =
      UnOp(UnOp.FileExtL(), x)  // ! simplify and use lower case here

    /** Replaces the extension part of this file. Parameter `s` may or may not contain a leading period. */
    def replaceExt(s: Ex[String]): Ex[File] =
      BinOp(BinOp.FileReplaceExt(), x, s)

    /** Replaces the name part of this file, keeping the parent directory. */
    def replaceName(s: Ex[String]): Ex[File] =
      BinOp(BinOp.FileReplaceName(), x, s)

    def / (child: Ex[String]): Ex[File] =
      BinOp(BinOp.FileChild(), x, child)
  }
}