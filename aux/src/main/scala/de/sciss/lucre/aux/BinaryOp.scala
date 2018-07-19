/*
 *  BinaryOp.scala
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

package de.sciss.lucre.aux

import de.sciss.lucre.aux.Aux.{Num, NumDouble, NumFrac, NumInt, Ord}
import de.sciss.lucre.stm.Base
import de.sciss.serial.{DataInput, DataOutput}

import scala.language.higherKinds

object BinaryOp {
  abstract class Pure[A1, A2] extends BinaryOp[A1, A2] {
    final type State[S <: Base[S]] = Unit

    final def readState   [S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S] = ()
    final def writeState  [S <: Base[S]](s: State[S], out: DataOutput): Unit = ()
    final def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit = ()

    final def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S] = ()

    def next[S <: Base[S]](a: A1, b: A1)(implicit state: State[S], tx: S#Tx): A2 = apply(a, b)

    def apply(a: A1, b: A1): A2
  }

  // ---- (Num, Num) -> Num ----

  final case class Plus[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.+(a, b)
    def name                  : String    = "Plus"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Minus[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.-(a, b)
    def name                  : String    = "Minus"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Times[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.*(a, b)
    def name                  : String    = "Times"
    def aux                   : List[Aux] = num :: Nil
  }

  /** Division, _not_ integer division */
  final case class Div[A]()(implicit num: NumFrac[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num./(a, b)
    def name                  : String    = "Div"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class ModJ[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.%(a, b)
    def name                  : String    = "ModJ"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Mod[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.mod(a, b)
    def name                  : String    = "Mod"
    def aux                   : List[Aux] = num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B }) extends Pure[A, B] {
    def apply(a: A, b: A)     : B           = eq.eq(a, b)
    def name                  : String      = "Eq"
    def aux                   : List[Aux]   = eq :: Nil
  }

  /** Not equal */
  final case class Neq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B}) extends Pure[A, B] {
    def apply(a: A, b: A)     : B           = eq.neq(a, b)
    def name                  : String      = "Neq"
    def aux                   : List[Aux]   = eq :: Nil
  }

  /** Less than */
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Pure[A, B] {
    def apply(a: A, b: A)     : B           = ord.lt(a, b)
    def name                  : String      = "Lt"
    def aux                   : List[Aux]   = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Pure[A, B] {
    def apply(a: A, b: A)     : B           = ord.gt(a, b)
    def name                  : String      = "Gt"
    def aux                   : List[Aux]   = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Pure[A, B] {
    def apply(a: A, b: A)     : B           = ord.leq(a, b)
    def name                  : String      = "Leq"
    def aux                   : List[Aux]   = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Pure[A, B] {
    def apply(a: A, b: A)     : B           = ord.geq(a, b)
    def name                  : String      = "Geq"
    def aux                   : List[Aux]   = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  final case class Min[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.min(a, b)
    def name                  : String    = "Min"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Max[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.max(a, b)
    def name                  : String    = "Max"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class BitAnd[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.&(a, b)
    def name                  : String    = "BitAnd"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class BitOr[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.|(a, b)
    def name                  : String    = "BitOr"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class BitXor[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.^(a, b)
    def name                  : String    = "BitXor"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Lcm[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.lcm(a, b)
    def name                  : String    = "Lcm"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Gcd[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.gcd(a, b)
    def name                  : String    = "Gcd"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class RoundTo[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.roundTo(a, b)
    def name                  : String    = "RoundTo"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class RoundUpTo[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.roundUpTo(a, b)
    def name                  : String    = "RoundUpTo"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Trunc[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.trunc(a, b)
    def name                  : String    = "Trunc"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Atan2[A]()(implicit num: NumDouble[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.atan2(a, b)
    def name                  : String    = "Atan2"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Hypot[A]()(implicit num: NumDouble[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.hypot(a, b)
    def name                  : String    = "Hypot"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Hypotx[A]()(implicit num: NumDouble[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.hypotApx(a, b)
    def name                  : String    = "Hypotx"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Pow[A]()(implicit num: NumDouble[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.pow(a, b)
    def name                  : String    = "Pow"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class LeftShift[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.<<(a, b)
    def name                  : String    = "LeftShift"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class RightShift[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.>>(a, b)
    def name                  : String    = "RightShift"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class UnsignedRightShift[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.>>>(a, b)
    def name                  : String    = "UnsignedRightShift"
    def aux                   : List[Aux] = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  final case class Difsqr[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.difSqr(a, b)
    def name                  : String    = "Difsqr"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Sumsqr[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.sumSqr(a, b)
    def name                  : String    = "Sumsqr"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Sqrsum[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrSum(a, b)
    def name                  : String    = "Sqrsum"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Sqrdif[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrDif(a, b)
    def name                  : String    = "Sqrdif"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Absdif[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.absDif(a, b)
    def name                  : String    = "Absdif"
    def aux                   : List[Aux] = num :: Nil
  }

  //  Thresh
  //  Amclip
  //  Scaleneg

  final case class Clip2[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.clip2(a, b)
    def name                  : String    = "Clip2"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Excess[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.excess(a, b)
    def name                  : String    = "Excess"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Fold2[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.fold2(a, b)
    def name                  : String    = "Fold2"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Wrap2[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A, b: A)     : A         = num.wrap2(a, b)
    def name                  : String    = "Wrap2"
    def aux                   : List[Aux] = num :: Nil
  }
}
sealed abstract class BinaryOp[A1, A2] extends ProductWithAux {
  type State[S <: Base[S]]

  def readState   [S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S]
  def writeState  [S <: Base[S]](s: State[S], out: DataOutput): Unit
  def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit

  def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S]

  def next[S <: Base[S]](a: A1, b: A1)(implicit state: State[S], tx: S#Tx): A2

  override final def productPrefix = s"BinaryOp$$$name"

  def name: String

  override def toString: String = name
}
