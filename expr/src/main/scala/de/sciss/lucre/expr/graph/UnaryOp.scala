/*
 *  UnaryOp.scala
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
package graph

import de.sciss.file._
import de.sciss.lucre.aux.Aux.{Num, NumBool, NumFrac, NumInt, ToNum, Widen, WidenToDouble}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.ITargets
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.expr.graph.UnaryOp.Op
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change
import de.sciss.span.{Span, SpanLike}

object UnaryOp {
  abstract class Op[A1, A2] extends Product {
    def apply(a: A1): A2
  }

  abstract class NamedOp[A1, A2] extends Op[A1, A2] {
    override def productPrefix = s"UnaryOp$$$name"

    override def toString: String = name

    def name: String
  }
  
  type AuxL = scala.List[Aux]

  // ---- analogous to UGens ----

  final case class Neg[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.negate(a)

    override def name = "Neg"

    override def aux: AuxL = num :: Nil
  }

  final case class Not[A]()(implicit num: NumBool[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.unary_!(a)
    
    override def name = "Not"
    
    override def aux: AuxL = num :: Nil
  }

  final case class BitNot[A]()(implicit num: NumInt[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.unary_~(a)
    
    override def name = "BitNot"
    
    override def aux: AuxL = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.abs(a)
    
    override def name = "Abs"
    
    override def aux: AuxL = num :: Nil
  }

  final case class ToDouble[A, B]()(implicit to: ToNum[A] { type Double = B }) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = to.toDouble(a)
    
    override def name = "ToDouble"
    
    override def aux: AuxL = to :: Nil
  }

  final case class ToInt[A, B]()(implicit to: ToNum[A] { type Int = B }) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = to.toInt(a)

    override def name = "ToInt"

    override def aux: AuxL = to :: Nil
  }

  final case class ToLong[A, B]()(implicit to: ToNum[A] { type Long = B }) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = to.toLong(a)

    override def name = "ToLong"

    override def aux: AuxL = to :: Nil
  }

  final case class Ceil[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.ceil(a)
    
    override def name = "Ceil"
    
    override def aux: AuxL = num :: Nil
  }

  final case class Floor[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.floor(a)

    override def name = "Floor"

    override def aux: AuxL = num :: Nil
  }

  final case class Frac[A]()(implicit num: NumFrac[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.frac(a)

    override def name = "Frac"

    override def aux: AuxL = num :: Nil
  }

  final case class Signum[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.signum(a)

    override def name = "Signum"

    override def aux: AuxL = num :: Nil
  }

  final case class Squared[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.squared(a)

    override def name = "Squared"

    override def aux: AuxL = num :: Nil
  }

  final case class Cubed[A]()(implicit num: Num[A]) extends NamedOp[A, A] with ProductWithAux {
    def apply(a: A): A = num.cubed(a)

    override def name = "Cubed"

    override def aux: AuxL = num :: Nil
  }

  final case class Sqrt[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.sqrt(wd.widen1(a))

    override def name = "Sqrt"

    override def aux: AuxL = wd :: Nil
  }

  final case class Exp[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.exp(wd.widen1(a))

    override def name = "Exp"

    override def aux: AuxL = wd :: Nil
  }

  final case class Reciprocal[A, B]()(implicit w: Widen[A, B], num: NumFrac[B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = num.reciprocal(w.widen1(a))

    override def name = "Reciprocal"

    override def aux: AuxL = w :: num :: Nil
  }

  final case class Midicps[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.midiCps(wd.widen1(a))

    override def name = "Midicps"

    override def aux: AuxL = wd :: Nil
  }

  final case class Cpsmidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.cpsMidi(wd.widen1(a))

    override def name = "Cpsmidi"

    override def aux: AuxL = wd :: Nil
  }

  final case class Midiratio[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.midiRatio(wd.widen1(a))

    override def name = "Midiratio"

    override def aux: AuxL = wd :: Nil
  }

  final case class Ratiomidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.ratioMidi(wd.widen1(a))

    override def name = "Ratiomidi"

    override def aux: AuxL = wd :: Nil
  }

  final case class Dbamp[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.dbAmp(wd.widen1(a))

    override def name = "Dbamp"

    override def aux: AuxL = wd :: Nil
  }

  final case class Ampdb[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.ampDb(wd.widen1(a))

    override def name = "Ampdb"

    override def aux: AuxL = wd :: Nil
  }

  final case class Octcps[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.octCps(wd.widen1(a))

    override def name = "Octcps"

    override def aux: AuxL = wd :: Nil
  }

  final case class Cpsoct[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.cpsOct(wd.widen1(a))

    override def name = "Cpsoct"

    override def aux: AuxL = wd :: Nil
  }

  final case class Log[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.log(wd.widen1(a))

    override def name = "Log"

    override def aux: AuxL = wd :: Nil
  }

  final case class Log2[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.log2(wd.widen1(a))

    override def name = "Log2"

    override def aux: AuxL = wd :: Nil
  }

  final case class Log10[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.log10(wd.widen1(a))

    override def name = "Log10"

    override def aux: AuxL = wd :: Nil
  }

  final case class Sin[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.sin(wd.widen1(a))

    override def name = "Sin"

    override def aux: AuxL = wd :: Nil
  }

  final case class Cos[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.cos(wd.widen1(a))

    override def name = "Cos"

    override def aux: AuxL = wd :: Nil
  }

  final case class Tan[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.tan(wd.widen1(a))

    override def name = "Tan"

    override def aux: AuxL = wd :: Nil
  }

  final case class Asin[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.asin(wd.widen1(a))

    override def name = "Asin"

    override def aux: AuxL = wd :: Nil
  }

  final case class Acos[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.acos(wd.widen1(a))

    override def name = "Acos"

    override def aux: AuxL = wd :: Nil
  }

  final case class Atan[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.atan(wd.widen1(a))

    override def name = "Atan"

    override def aux: AuxL = wd :: Nil
  }

  final case class Sinh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.sinh(wd.widen1(a))

    override def name = "Sinh"

    override def aux: AuxL = wd :: Nil
  }

  final case class Cosh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.cosh(wd.widen1(a))

    override def name = "Cosh"

    override def aux: AuxL = wd :: Nil
  }

  final case class Tanh[A, B]()(implicit wd: WidenToDouble[A, B]) extends NamedOp[A, B] with ProductWithAux {
    def apply(a: A): B = wd.tanh(wd.widen1(a))

    override def name = "Tanh"

    override def aux: AuxL = wd :: Nil
  }

//  final case class Rand[A]()(implicit num: Num[A])

//  final case class Rand2

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand

  // Distort
  // Softclip

//  final case class Coin[A, B]()(implicit num: NumDouble[A] { type Boolean = B })

  // RectWindow
  // HanWindow
  // WelWindow
  // TriWindow

  // Ramp
  // Scurve

  // ---- general ----

  final case class ToStr[A]() extends NamedOp[A, String] {
    def apply(a: A) : String = a.toString

    override def name = "ToStr"
  }

  // ---- Option ----

  final case class OptionSome[A]() extends NamedOp[A, Option[A]] {
    def apply(a: A): Option[A] = Some(a)

    override def name = "OptionSome"
  }

  final case class OptionIsEmpty[A]() extends NamedOp[Option[A], Boolean] {
    def apply(a: Option[A]): Boolean = a.isEmpty

    override def name = "OptionIsEmpty"
  }

  final case class OptionIsDefined[A]() extends NamedOp[Option[A], Boolean] {
    def apply(a: Option[A]): Boolean = a.isDefined

    override def name = "OptionIsDefined"
  }

  final case class OptionToList[A]() extends NamedOp[Option[A], scala.List[A]] {
    def apply(a: Option[A]): scala.List[A] = a.toList

    override def name = "OptionToList"
  }

  // ---- Seq ----

  final case class SeqSize[A]() extends NamedOp[Seq[A], Int] {
    def apply(a: Seq[A]): Int = a.size

    override def name = "SeqSize"
  }

  final case class SeqHeadOption[A]() extends NamedOp[Seq[A], Option[A]] {
    def apply(a: Seq[A]): Option[A] = a.headOption

    override def name = "SeqHeadOption"
  }

  final case class SeqLastOption[A]() extends NamedOp[Seq[A], Option[A]] {
    def apply(a: Seq[A]): Option[A] = a.lastOption

    override def name = "SeqLastOption"
  }

  final case class SeqIsEmpty[A]() extends NamedOp[Seq[A], Boolean] {
    def apply(a: Seq[A]): Boolean = a.isEmpty

    override def name = "SeqIsEmpty"
  }

  final case class SeqNonEmpty[A]() extends NamedOp[Seq[A], Boolean] {
    def apply(a: Seq[A]): Boolean = a.nonEmpty

    override def name = "SeqNonEmpty"
  }

  // ---- String ----

  final case class StringIsEmpty() extends NamedOp[String, Boolean] {
    def apply(a: String): Boolean = a.isEmpty

    override def name = "StringIsEmpty"
  }

  final case class StringNonEmpty() extends NamedOp[String, Boolean] {
    def apply(a: String): Boolean = !a.isEmpty
    override def name = "StringNonEmpty"
  }

  final case class StringLength() extends NamedOp[String, Int] {
    def apply(a: String): Int = a.length

    override def name = "StringLength"
  }

  // ---- SpanLike ----

  final case class SpanLikeIsEmpty() extends NamedOp[SpanLike, Boolean] {
    def apply(a: SpanLike): Boolean = a.isEmpty

    override def name = "SpanLikeIsEmpty"
  }

  final case class SpanLikeNonEmpty() extends NamedOp[SpanLike, Boolean] {
    def apply(a: SpanLike): Boolean = a.nonEmpty

    override def name = "SpanLikeNonEmpty"
  }

  final case class SpanLikeClosedOption() extends NamedOp[SpanLike, Option[Span]] {
    def apply(a: SpanLike): Option[Span] = a match {
      case sp: Span => Some(sp)
      case _        => None
    }

    override def name = "SpanLikeClosedOption"
  }

  final case class SpanLikeStartOption() extends NamedOp[SpanLike, Option[Long]] {
    def apply(a: SpanLike): Option[Long] = a.startOption

    override def name = "SpanLikeStartOption"
  }

  final case class SpanLikeStopOption() extends NamedOp[SpanLike, Option[Long]] {
    def apply(a: SpanLike): Option[Long] = a.stopOption

    override def name = "SpanLikeStopOption"
  }

  final case class SpanLikeLengthOption() extends NamedOp[SpanLike, Option[Long]] {
    def apply(a: SpanLike): Option[Long] = a match {
      case sp: Span.SpanOrVoid  => Some(sp.length)
      case _                    => None
    }

    override def name = "SpanLikeLengthOption"
  }

  // ---- Span ----

  final case class SpanStart() extends NamedOp[Span, Long] {
    def apply(a: Span): Long = a.start

    override def name = "SpanStart"
  }

  final case class SpanStop() extends NamedOp[Span, Long] {
    def apply(a: Span): Long = a.stop

    override def name = "SpanStop"
  }

  final case class SpanLength() extends NamedOp[Span, Long] {
    def apply(a: Span): Long = a.length

    override def name = "SpanLength"
  }

  // ---- File ----

  final case class FileParentOption() extends NamedOp[File, Option[File]] {
    def apply(a: File): Option[File] = a.parentOption

    def name = "FileParentOption"
  }

  final case class FilePath() extends NamedOp[File, String] {
    def apply(a: File): String = a.path

    def name = "FilePath"
  }

  final case class FileName() extends NamedOp[File, String] {
    def apply(a: File): String = a.name

    def name = "FileName"
  }

  final case class FileBase() extends NamedOp[File, String] {
    def apply(a: File): String = a.base

    def name = "FileBase"
  }

  final case class FileExtL() extends NamedOp[File, String] {
    def apply(a: File): String = a.extL

    def name = "FileExtL"
  }

  // ---- Impl ----

  private[graph] final class Expanded[S <: Base[S], A1, A](op: Op[A1, A], a: IExpr[S, A1], tx0: S#Tx)
                                                          (implicit targets: ITargets[S])
    extends MappedIExpr[S, A1, A](a, tx0) with IEventImpl[S, Change[A]] {

    override def toString: String = s"UnaryOp($op, $a)"

    protected def mapValue(av: A1): A = op(av)
  }
}

final case class UnaryOp[A1, A](op: Op[A1, A], a: Ex[A1])
  extends Ex[A] {

  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val ax = a.expand[S]
    new UnaryOp.Expanded[S, A1, A](op, ax, tx)
  }
}
