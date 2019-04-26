/*
 *  BinaryOp.scala
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

import de.sciss.lucre.aux.Aux.{Num, NumDouble, NumFrac, NumInt, Ord, Widen2}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

import scala.collection.immutable.{Seq => ISeq}

object BinaryOp {
  sealed abstract class Op[A, B, C] extends ProductWithAux {
    def apply(a: A, b: B): C

    override final def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  // ---- (Num, Num) -> Num ----

  final case class Plus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.+(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Plus"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Minus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.-(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Minus"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Times[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.*(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Times"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  /** Division, _not_ integer division */
  final case class Div[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumFrac[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num./(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Div"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class ModJ[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.%(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "ModJ"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Mod[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.mod(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Mod"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B }) extends Op[A, A, B] {
    def apply(a: A, b: A)     : B           = eq.eq(a, b)
    def name                  : String      = "Eq"
    def aux                   : scala.List[Aux]   = eq :: Nil
  }

  /** Not equal */
  final case class Neq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B}) extends Op[A, A, B] {
    def apply(a: A, b: A)     : B           = eq.neq(a, b)
    def name                  : String      = "Neq"
    def aux                   : scala.List[Aux]   = eq :: Nil
  }

  /** Less than */
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, A, B] {
    def apply(a: A, b: A)     : B           = ord.lt(a, b)
    def name                  : String      = "Lt"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, A, B] {
    def apply(a: A, b: A)     : B           = ord.gt(a, b)
    def name                  : String      = "Gt"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, A, B] {
    def apply(a: A, b: A)     : B           = ord.leq(a, b)
    def name                  : String      = "Leq"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, A, B] {
    def apply(a: A, b: A)     : B           = ord.geq(a, b)
    def name                  : String      = "Geq"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  final case class Min[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.min(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Min"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Max[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.max(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Max"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class BitAnd[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.&(a, b)
    def name                  : String    = "BitAnd"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class BitOr[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.|(a, b)
    def name                  : String    = "BitOr"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class BitXor[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.^(a, b)
    def name                  : String    = "BitXor"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Lcm[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.lcm(a, b)
    def name                  : String    = "Lcm"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Gcd[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.gcd(a, b)
    def name                  : String    = "Gcd"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class RoundTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.roundTo(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "RoundTo"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class RoundUpTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.roundUpTo(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "RoundUpTo"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Trunc[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.trunc(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Trunc"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Atan2[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.atan2(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Atan2"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Hypot[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.hypot(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Hypot"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Hypotx[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.hypotApx(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Hypotx"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Pow[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.pow(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Pow"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class LeftShift[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.<<(a, b)
    def name                  : String    = "LeftShift"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class RightShift[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.>>(a, b)
    def name                  : String    = "RightShift"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class UnsignedRightShift[A]()(implicit num: NumInt[A]) extends Op[A, A, A] {
    def apply(a: A, b: A)     : A         = num.>>>(a, b)
    def name                  : String    = "UnsignedRightShift"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  final case class Difsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.difSqr(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Difsqr"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Sumsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.sumSqr(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Sumsqr"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Sqrsum[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.sqrSum(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Sqrsum"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Sqrdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.sqrDif(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Sqrdif"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Absdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.absDif(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Absdif"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  //  Thresh
  //  Amclip
  //  Scaleneg

  final case class Clip2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.clip2(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Clip2"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Excess[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.excess(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Excess"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Fold2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.fold2(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Fold2"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  final case class Wrap2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) extends Op[A, B, C] {
    def apply(a: A, b: B)     : C         = num.wrap2(widen.widen1(a), widen.widen2(b))
    def name                  : String    = "Wrap2"
    def aux                   : scala.List[Aux] = widen :: num :: Nil
  }

  // ---- Option ----

  final case class OptionContains[A]() extends Op[Option[A], A, Boolean] {
    def apply(a: Option[A], b: A) : Boolean = a.contains(b)
    def name                      : String  = "OptionContains"
    def aux : scala.List[Aux] = Nil
  }

  final case class OptionGetOrElse[A]() extends Op[Option[A], A, A] {
    def apply(a: Option[A], b: A) : A       = a.getOrElse(b)
    def name                      : String  = "OptionGetOrElse"
    def aux : scala.List[Aux] = Nil
  }

  final case class OptionOrElse[A]() extends Op[Option[A], Option[A], Option[A]] {
    def apply(a: Option[A], b: Option[A]) : Option[A] = a.orElse(b)
    def name                              : String    = "OptionGetOrElse"
    def aux : scala.List[Aux] = Nil
  }

  // ---- Seq ----

  final case class SeqApplyOption[A]() extends Op[ISeq[A], Int, Option[A]] {
    def apply(a: ISeq[A], b: Int): Option[A] = if (b < 0) None else {
      if (a.lengthCompare(b) <= 0) None else Some(a.apply(b))
    }

    def name: String          = "SeqApplyOption"
    def aux : scala.List[Aux] = Nil
  }

  // ---- String ----

  final case class StringConcat() extends Op[String, String, String] {
    def apply(a: String, b: String): String = a + b

    def name: String          = "StringConcat"
    def aux : scala.List[Aux] = Nil
  }

  // ---- Impl ----

  private final class Expanded[S <: Base[S], A1, A2, A3, A](op: BinaryOp.Op[A1, A2, A],
                                                            a: IExpr[S, A1], b: IExpr[S, A2], tx0: S#Tx)
                                                           (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IEventImpl[S, Change[A]] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)

    override def toString: String = s"BinaryOp($op, $a, $b)"

    def changed: IEvent[S, Change[A]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
      val _1c = a.changed
      val _2c = b.changed

      val _1ch = if (pull.contains(_1c)) pull(_1c) else None
      val _2ch = if (pull.contains(_2c)) pull(_2c) else None

      (_1ch, _2ch) match {
        case (Some(ach), None) =>
          val bv      = b.value
          val before  = value1(ach.before, bv)
          val now     = value1(ach.now, bv)
          if (before == now) None else Some(Change(before, now))
        case (None, Some(bch)) =>
          val av      = a.value
          val before  = value1(av, bch.before)
          val now     = value1(av, bch.now)
          if (before == now) None else Some(Change(before, now))
        case (Some(ach), Some(bch)) =>
          val before  = value1(ach.before, bch.before)
          val now     = value1(ach.now, bch.now)
          if (before == now) None else Some(Change(before, now))
        case _ => None
      }
    }

    @inline
    private def value1(av: A1, bv: A2): A = {
      op.apply(av, bv)
    }

    def value(implicit tx: S#Tx): A = {
      val av = a.value
      val bv = b.value
      value1(av, bv)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
    }
  }
}
final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A1, A2, A], a: Ex[A1], b: Ex[A2])
  extends Ex.Lazy[A] {

  protected def mkExpr[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] = {
    import ctx.targets
    val ax = a.expand[S]
    val bx = b.expand[S]
    new BinaryOp.Expanded[S, A1, A2, A3, A](op, ax, bx, tx)
  }
}
