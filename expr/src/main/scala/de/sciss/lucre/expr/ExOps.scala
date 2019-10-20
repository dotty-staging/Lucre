/*
 *  ExOps.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import java.io.{File => _File}

import de.sciss.lucre.adjunct.Adjunct.{Eq, Num, NumBool, NumDouble, NumFrac, NumInt, Ord, ToNum, Widen, Widen2, WidenToDouble}
import de.sciss.lucre.expr.graph.{Act, Attr, Changed, Ex, File, Latch, Obj, QuaternaryOp, ToTrig, Trig, BinaryOp => BinOp, TernaryOp => TernOp, UnaryOp => UnOp}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

// XXX TODO --- use constant optimizations
final class ExOps[A](private val x: Ex[A]) extends AnyVal {
  // unary element-wise

  def unary_-   (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Neg[A](), x)
  def unary_!   (implicit num: NumBool[A] ): Ex[A]          = UnOp(UnOp.Not[A](), x)
  def unary_~   (implicit num: NumInt[A]  ): Ex[A]          = UnOp(UnOp.BitNot[A](), x)
  def abs       (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Abs[A](), x)

  def toDouble  (implicit to: ToNum[A]): Ex[to.Double]      = UnOp(UnOp.ToDouble[A, to.Double]()(to), x)
  def toInt     (implicit to: ToNum[A]): Ex[to.Int]         = UnOp(UnOp.ToInt   [A, to.Int   ]()(to), x)
  def toLong    (implicit to: ToNum[A]): Ex[to.Long]        = UnOp(UnOp.ToLong  [A, to.Long  ]()(to), x)

  def ceil      (implicit num: NumFrac[A] ): Ex[A]          = UnOp(UnOp.Ceil    [A](), x)
  def floor     (implicit num: NumFrac[A] ): Ex[A]          = UnOp(UnOp.Floor   [A](), x)
  def frac      (implicit num: NumFrac[A] ): Ex[A]          = UnOp(UnOp.Frac    [A](), x)
  def signum    (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Signum  [A](), x)
  def squared   (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Squared [A](), x)
  def cubed     (implicit num: Num[A]     ): Ex[A]          = UnOp(UnOp.Cubed   [A](), x)

  def sqrt   [B](implicit wd: WidenToDouble[A, B]): Ex[B]   = UnOp(UnOp.Sqrt[A, B](), x)
  def exp    [B](implicit wd: WidenToDouble[A, B]): Ex[B]   = UnOp(UnOp.Exp [A, B](), x)

  def reciprocal[B](implicit w: Widen[A, B], num: NumFrac[B]): Ex[B] = UnOp(UnOp.Reciprocal[A, B](), x)

  def midiCps  [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Midicps   [A, B](), x)
  def cpsMidi  [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cpsmidi   [A, B](), x)
  def midiRatio[B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Midiratio [A, B](), x)
  def ratioMidi[B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Ratiomidi [A, B](), x)
  def dbAmp    [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Dbamp     [A, B](), x)
  def ampDb    [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Ampdb     [A, B](), x)

  def octCps   [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Octcps    [A, B](), x)
  def cpsOct   [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cpsoct    [A, B](), x)
  def log      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Log       [A, B](), x)
  def log2     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Log2      [A, B](), x)
  def log10    [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Log10     [A, B](), x)
  def sin      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Sin       [A, B](), x)
  def cos      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cos       [A, B](), x)
  def tan      [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Tan       [A, B](), x)
  def asin     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Asin      [A, B](), x)
  def acos     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Acos      [A, B](), x)
  def atan     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Atan      [A, B](), x)
  def sinh     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Sinh      [A, B](), x)
  def cosh     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Cosh      [A, B](), x)
  def tanh     [B](implicit wd: WidenToDouble[A, B]): Ex[B] = UnOp(UnOp.Tanh      [A, B](), x)

//  def rand      (implicit num: Num[A]       ): Ex[A]           = UnOp(UnOp.Rand  [A](), x)
//  def rand2     (implicit num: Num[A]       ): Ex[A]           = UnOp(UnOp.Rand2 [A](), x)
//  def coin      (implicit num: NumDouble[A] ): Ex[num.Boolean] = UnOp(UnOp.Coin  [A, num.Boolean]()(num), x)

  def toStr: Ex[String] = UnOp(UnOp.ToStr[A](), x)

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A])(implicit eq: Eq[A]): Ex[eq.Boolean] = BinOp(BinOp.Eq [A, eq.Boolean]()(eq), x, that)
  def sig_!= (that: Ex[A])(implicit eq: Eq[A]): Ex[eq.Boolean] = BinOp(BinOp.Neq[A, eq.Boolean]()(eq), x, that)

  def <  (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Lt [A, ord.Boolean]()(ord), x, that)
  def >  (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Gt [A, ord.Boolean]()(ord), x, that)
  def <= (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Leq[A, ord.Boolean]()(ord), x, that)
  def >= (that: Ex[A])(implicit ord: Ord[A]): Ex[ord.Boolean] = BinOp(BinOp.Geq[A, ord.Boolean]()(ord), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

  def &   (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
  def |   (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
  def ^   (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

  /** Currently a shortcut for `&`. */
  def &&  (that: Ex[A])(implicit num: NumBool[A]): Ex[A] = BinOp(BinOp.And   [A](), x, that)
  /** Currently a shortcut for `|`. */
  def ||  (that: Ex[A])(implicit num: NumBool[A]): Ex[A] = BinOp(BinOp.Or    [A](), x, that)

  def lcm (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
  def gcd (that: Ex[A])(implicit num: NumInt [A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

  def <<  (that: Ex[A])(implicit num: NumInt[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  def >>  (that: Ex[A])(implicit num: NumInt[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
  def >>> (that: Ex[A])(implicit num: NumInt[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

  //  def <--- (attr: ExAttrLike[A]): Unit = ...
  //  def <--> (attr: ExAttrLike[A]): Unit = ...

//  def linLin[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
//                    (implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] =
//    LinLin[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)
//
//  def linExp[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
//                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Ex[A2] =
//    LinExp[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)
//
//  def expLin[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
//                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Ex[A2] =
//    ExpLin[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)
//
//  def expExp[A1, A2](inLo: Ex[A], inHi: Ex[A], outLo: Ex[A1], outHi: Ex[A1])
//                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Ex[A2] =
//    ExpExp[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)
//
//  def poll(label: Ex[String] = "poll", gate: Ex[Boolean] = true): Ex[A] =
//    Poll(x, gate = gate, label = label)

  // ---- bridge to trigger ----

  def changed: Trig = Changed(x)

  /** Latches the expression based on the trigger argument.
    * The initial state of the returned expression corresponds to the
    * initial state of the input expression. Subsequent values are
    * updated and cached only when a trigger occurs.
    */
  def latch(tr: Trig): Ex[A] = Latch(x, tr)

  /** Alias for `latch` */
  def <| (tr: Trig): Ex[A] = Latch(x, tr)

  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

final class ExBooleanOps(private val x: Ex[Boolean]) extends AnyVal {
  def toTrig: Trig = ToTrig(x)
}

final class ExStringOps(private val x: Ex[String]) extends AnyVal {
  def length  : Ex[Int]     = UnOp(UnOp.StringLength(), x)
  def size    : Ex[Int]     = length

  def isEmpty : Ex[Boolean] = UnOp(UnOp.StringIsEmpty(), x)
  def nonEmpty: Ex[Boolean] = UnOp(UnOp.StringNonEmpty(), x)

  def ++      (that: Ex[String]): Ex[String]  = BinOp(BinOp.StringConcat  (), x, that)
  def contains(that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringContains(), x, that)
  def indexOf (that: Ex[String]): Ex[Int]     = BinOp(BinOp.StringIndexOf (), x, that)

  def take(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringTake(), x, n)
  def drop(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringDrop(), x, n)
  
  def slice(from: Ex[Int], until: Ex[Int]): Ex[String] = TernOp(TernOp.StringSlice(), x, from, until)

  /** Applies 'printf' style formatting. See `StringFormat` for details.
    */
  def format(args: Ex[Any]*): Ex[String] = graph.StringFormat(x, args)
}

final class ExSeqOps[A](private val x: Ex[Seq[A]]) extends AnyVal {
  //  def apply(index: Ex[Int]): Ex[A] = ...

  def applyOption(index: Ex[Int]): Ex[Option[A]] = BinOp(BinOp.SeqApplyOption[A](), x, index)

  def headOption: Ex[Option[A]] = UnOp(UnOp.SeqHeadOption[A](), x)
  def lastOption: Ex[Option[A]] = UnOp(UnOp.SeqLastOption[A](), x)

  def size: Ex[Int] = UnOp(UnOp.SeqSize[A](), x)

  def isEmpty : Ex[Boolean] = UnOp(UnOp.SeqIsEmpty  [A](), x)
  def nonEmpty: Ex[Boolean] = UnOp(UnOp.SeqNonEmpty [A](), x)

  def ++ [B >: A](that: Ex[Seq[B]]): Ex[Seq[B]] = BinOp(BinOp.SeqConcat[B](), x, that)

  def take(n: Ex[Int]): Ex[Seq[A]] = BinOp(BinOp.SeqTake[A](), x, n)
  def drop(n: Ex[Int]): Ex[Seq[A]] = BinOp(BinOp.SeqDrop[A](), x, n)

  def slice(from: Ex[Int], until: Ex[Int]): Ex[Seq[A]] = TernOp(TernOp.SeqSlice[A](), x, from, until)

  def zip[B](that: Ex[Seq[B]]): Ex[Seq[(A, B)]] = BinOp(BinOp.SeqZip[A, B](), x, that)

  def map[B, To](f: Ex[A] => B)(implicit m: Ex.CanMap[Seq, B, To]): To =
    m.map(x, f)

  def flatMap[B, To](f: Ex[A] => B)(implicit fm: Ex.CanFlatMap[Seq, B, To]): To =
    fm.flatMap(x, f)

  def mkString(sep: Ex[String]): Ex[String] = {
    import Ex.const
    mkString("", sep, "")
  }

  def mkString(start: Ex[String], sep: Ex[String], end: Ex[String]): Ex[String] =
    QuaternaryOp(QuaternaryOp.SeqMkString[A](), x, start, sep, end)
}

final class ExSpanOps[A <: _SpanLike](private val x: Ex[A]) extends AnyVal {
  def clip(pos: Ex[Long]): Ex[Long] = BinOp(BinOp.SpanLikeClip(), x, pos)

  def shift(delta: Ex[Long]): Ex[_SpanLike] = BinOp(BinOp.SpanLikeShift(), x, delta)

  def isEmpty : Ex[Boolean] = UnOp(UnOp.SpanLikeIsEmpty (), x)
  def nonEmpty: Ex[Boolean] = UnOp(UnOp.SpanLikeNonEmpty(), x)

  def contains(pos: Ex[Long]): Ex[Boolean] = BinOp(BinOp.SpanLikeContains(), x, pos)

// cannot overload due to erasure
//  def contains(that: Ex[SpanLike]): Ex[Boolean]

  def overlaps  (that: Ex[_SpanLike]): Ex[Boolean]    = BinOp(BinOp.SpanLikeOverlaps  (), x, that)
  def touches   (that: Ex[_SpanLike]): Ex[Boolean]    = BinOp(BinOp.SpanLikeTouches   (), x, that)

  def union     (that: Ex[_SpanLike]): Ex[_SpanLike]  = BinOp(BinOp.SpanLikeUnion     (), x, that)
  def intersect (that: Ex[_SpanLike]): Ex[_SpanLike]  = BinOp(BinOp.SpanLikeIntersect (), x, that)

  def closedOption: Ex[Option[_Span]] = UnOp(UnOp.SpanLikeClosedOption(), x)
  def startOption : Ex[Option[Long]]  = UnOp(UnOp.SpanLikeStartOption (), x)
  def stopOption  : Ex[Option[Long]]  = UnOp(UnOp.SpanLikeStopOption  (), x)
  def lengthOption: Ex[Option[Long]]  = UnOp(UnOp.SpanLikeLengthOption(), x)

  // ---- simplify to only span here ----

  def start (implicit ev: Ex[A] =:= Ex[_Span]): Ex[Long] = UnOp(UnOp.SpanStart (), ev(x))
  def stop  (implicit ev: Ex[A] =:= Ex[_Span]): Ex[Long] = UnOp(UnOp.SpanStop  (), ev(x))
  def length(implicit ev: Ex[A] =:= Ex[_Span]): Ex[Long] = UnOp(UnOp.SpanLength(), ev(x))
}

final class ExOptionOps[A](private val x: Ex[Option[A]]) extends AnyVal {
  def isEmpty   : Ex[Boolean] = UnOp(UnOp.OptionIsEmpty   [A](), x)
  def isDefined : Ex[Boolean] = UnOp(UnOp.OptionIsDefined [A](), x)
  def nonEmpty  : Ex[Boolean] = isDefined

  def getOrElse [B >: A](default    : Ex[B])        : Ex[B]         = BinOp(BinOp.OptionGetOrElse[B](), x, default)
  def orElse    [B >: A](alternative: Ex[Option[B]]): Ex[Option[B]] = BinOp(BinOp.OptionOrElse   [B](), x, alternative)

  def contains[B >: A](elem: Ex[B]): Ex[Boolean] = BinOp(BinOp.OptionContains[B](), x, elem)

  def toList: Ex[scala.List[A]] = UnOp(UnOp.OptionToList[A](), x)

  def map[B, To](f: Ex[A] => B)(implicit m: Ex.CanMap[Option, B, To]): To =
    m.map(x, f)

  def flatMap[B, To](f: Ex[A] => B)(implicit fm: Ex.CanFlatMap[Option, B, To]): To =
    fm.flatMap(x, f)
}

final class IntLiteralExOps(private val x: Int) extends AnyVal {
  private type A = Int

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
  //  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Eq [A, Boolean](), x, that)
  def sig_!= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Neq[A, Boolean](), x, that)

  def <  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Lt [A, Boolean](), x, that)
  def >  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Gt [A, Boolean](), x, that)
  def <= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Leq[A, Boolean](), x, that)
  def >= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Geq[A, Boolean](), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

  def &   (that: Ex[A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
  def |   (that: Ex[A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
  def ^   (that: Ex[A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

  def lcm (that: Ex[A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
  def gcd (that: Ex[A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

  //  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
  //  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
  //  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
  //  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

  def <<  (that: Ex[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  def >>  (that: Ex[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
  def >>> (that: Ex[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

  //  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

  //  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

final class LongLiteralExOps(private val x: Long) extends AnyVal {
  private type A = Long

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
//  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Eq [A, Boolean](), x, that)
  def sig_!= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Neq[A, Boolean](), x, that)

  def <  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Lt [A, Boolean](), x, that)
  def >  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Gt [A, Boolean](), x, that)
  def <= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Leq[A, Boolean](), x, that)
  def >= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Geq[A, Boolean](), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

  def &   (that: Ex[A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
  def |   (that: Ex[A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
  def ^   (that: Ex[A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

  def lcm (that: Ex[A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
  def gcd (that: Ex[A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

//  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
//  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
//  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
//  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

  def <<  (that: Ex[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  def >>  (that: Ex[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
  def >>> (that: Ex[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

//  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

//  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

final class DoubleLiteralExOps(private val x: Double) extends AnyVal {
  private type A = Double

  // binary

  def +  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Plus [A, A1, A2](), x, that)
  def -  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Minus[A, A1, A2](), x, that)
  def *  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Times[A, A1, A2](), x, that)
  def /  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Ex[A2] = BinOp(BinOp.Div  [A, A1, A2](), x, that)
  def %  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.ModJ [A, A1, A2](), x, that)
  def mod[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num    [A2]): Ex[A2] = BinOp(BinOp.Mod  [A, A1, A2](), x, that)

  def sig_== (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Eq [A, Boolean](), x, that)
  def sig_!= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Neq[A, Boolean](), x, that)

  def <  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Lt [A, Boolean](), x, that)
  def >  (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Gt [A, Boolean](), x, that)
  def <= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Leq[A, Boolean](), x, that)
  def >= (that: Ex[A]): Ex[Boolean] = BinOp(BinOp.Geq[A, Boolean](), x, that)

  def min[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Min[A, A1, A2](), x, that)
  def max[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Max[A, A1, A2](), x, that)

//  def &   (that: Ex[A]): Ex[A] = BinOp(BinOp.And[A](), x, that)
//  def |   (that: Ex[A]): Ex[A] = BinOp(BinOp.Or [A](), x, that)
//  def ^   (that: Ex[A]): Ex[A] = BinOp(BinOp.Xor[A](), x, that)

//  def lcm (that: Ex[A]): Ex[A] = BinOp(BinOp.Lcm   [A](), x, that)
//  def gcd (that: Ex[A]): Ex[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundTo  [A, A1, A2](), x, that)
  def roundUpTo [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.RoundUpTo[A, A1, A2](), x, that)
  def trunc     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Ex[A2] = BinOp(BinOp.Trunc    [A, A1, A2](), x, that)

  def atan2     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Atan2    [A, A1, A2](), x, that)
  def hypot     [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypot    [A, A1, A2](), x, that)
  def hypotApx  [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Hypotx   [A, A1, A2](), x, that)
  def pow       [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Ex[A2] = BinOp(BinOp.Pow      [A, A1, A2](), x, that)

//  def <<  (that: Ex[A]): Ex[A] = BinOp(BinOp.LeftShift         [A](), x, that)
//  def >>  (that: Ex[A]): Ex[A] = BinOp(BinOp.RightShift        [A](), x, that)
//  def >>> (that: Ex[A]): Ex[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Difsqr[A, A1, A2](), x, that)
  def sumSqr[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sumsqr[A, A1, A2](), x, that)
  def sqrSum[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrsum[A, A1, A2](), x, that)
  def sqrDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Sqrdif[A, A1, A2](), x, that)
  def absDif[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Absdif[A, A1, A2](), x, that)

  def clip2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Clip2 [A, A1, A2](), x, that)
  def excess[A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Excess[A, A1, A2](), x, that)
  def fold2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Fold2 [A, A1, A2](), x, that)
  def wrap2 [A1, A2](that: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = BinOp(BinOp.Wrap2 [A, A1, A2](), x, that)

  // ternary

  def clip[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Clip[A, A1, A2](), x, lo, hi)
  def fold[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Fold[A, A1, A2](), x, lo, hi)
  def wrap[A1, A2](lo: Ex[A1], hi: Ex[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Ex[A2] = TernOp(TernOp.Wrap[A, A1, A2](), x, lo, hi)

  //  def ---> (attr: Attr.Like[A]): Unit = attr.update(x)

  //  def asObj(implicit cm: Obj.CanMake[A]): Obj.Make = Obj.Make(x)
}

// XXX TODO --- we could check here if arguments are constants,
// and in that case fall back to constant behaviour like `scala.StringOps`.
/** Some methods are here form `ExStringOps` again, so that we can
  * use them on plain string literals, without requiring an
  * explicit wrap such as `Const("x")` first.
  */
final class StringLiteralExOps(private val x: String) extends AnyVal {
  def attr[A](implicit bridge: Obj.Bridge[A]): Attr[A] = Attr(x)

  def attr[A](default: Ex[A])(implicit bridge: Obj.Bridge[A]): Attr.WithDefault[A] =
    Attr.WithDefault(x, default)

  def size: Int /*Ex[Int]*/ = x.length

  def nonEmpty: Boolean /*Ex[Boolean]*/ = !x.isEmpty

  def ++ (that: Ex[String]): Ex[String] = BinOp(BinOp.StringConcat(), x, that)

  def contains(that: Ex[String]): Ex[Boolean] = BinOp(BinOp.StringContains(), x, that)
  def indexOf (that: Ex[String]): Ex[Int]     = BinOp(BinOp.StringIndexOf (), x, that)

  def take(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringTake(), x, n)
  def drop(n: Ex[Int]): Ex[String] = BinOp(BinOp.StringDrop(), x, n)

  def slice(from: Ex[Int], until: Ex[Int]): Ex[String] = TernOp(TernOp.StringSlice(), x, from, until)

  def format(args: Ex[Any]*): Ex[String] = graph.StringFormat(x, args)
}

final class ExTuple2Ops[A, B](private val x: Ex[(A, B)]) extends AnyVal {
  def _1: Ex[A] = UnOp(UnOp.Tuple2_1[A, B](), x)
  def _2: Ex[B] = UnOp(UnOp.Tuple2_2[A, B](), x)

  def swap: Ex[(B, A)] = UnOp(UnOp.Tuple2Swap[A, B](), x)
}

final class ExFileOps(private val x: Ex[_File]) extends AnyVal {
  // ---- expressions ----

  /** Returns the parent directory if it exists. */
  def parentOption: Ex[Option[_File]] =
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
  def replaceExt(s: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileReplaceExt(), x, s)

  /** Replaces the name part of this file, keeping the parent directory. */
  def replaceName(s: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileReplaceName(), x, s)

  def / (child: Ex[String]): Ex[_File] =
    BinOp(BinOp.FileChild(), x, child)

  // ---- actions ----

  /** Deletes the file */
  def delete: Act = File.Delete(x)

  /** Creates the directory and possibly parent directories denoted by this file. */
  def mkDir : Act = File.MkDir(x)
}
