package de.sciss.lucre.expr
package graph

import de.sciss.lucre.aux.Aux.{Num, NumDouble, NumFrac, NumInt, Ord, Widen2}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.Observable
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm.{Sys, TxnLike}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object BinaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    def apply(a: A1, b: A1): A2

    override final def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  // ---- (Num, Num) -> Num ----

  final case class Plus[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.+(a, b)
    def name                  : String    = "Plus"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Minus[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.-(a, b)
    def name                  : String    = "Minus"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Times[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.*(a, b)
    def name                  : String    = "Times"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  /** Division, _not_ integer division */
  final case class Div[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num./(a, b)
    def name                  : String    = "Div"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class ModJ[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.%(a, b)
    def name                  : String    = "ModJ"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Mod[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.mod(a, b)
    def name                  : String    = "Mod"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B }) extends Op[A, B] {
    def apply(a: A, b: A)     : B           = eq.eq(a, b)
    def name                  : String      = "Eq"
    def aux                   : scala.List[Aux]   = eq :: Nil
  }

  /** Not equal */
  final case class Neq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B}) extends Op[A, B] {
    def apply(a: A, b: A)     : B           = eq.neq(a, b)
    def name                  : String      = "Neq"
    def aux                   : scala.List[Aux]   = eq :: Nil
  }

  /** Less than */
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, B] {
    def apply(a: A, b: A)     : B           = ord.lt(a, b)
    def name                  : String      = "Lt"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, B] {
    def apply(a: A, b: A)     : B           = ord.gt(a, b)
    def name                  : String      = "Gt"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, B] {
    def apply(a: A, b: A)     : B           = ord.leq(a, b)
    def name                  : String      = "Leq"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) extends Op[A, B] {
    def apply(a: A, b: A)     : B           = ord.geq(a, b)
    def name                  : String      = "Geq"
    def aux                   : scala.List[Aux]   = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  final case class Min[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.min(a, b)
    def name                  : String    = "Min"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Max[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.max(a, b)
    def name                  : String    = "Max"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class BitAnd[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.&(a, b)
    def name                  : String    = "BitAnd"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class BitOr[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.|(a, b)
    def name                  : String    = "BitOr"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class BitXor[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.^(a, b)
    def name                  : String    = "BitXor"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Lcm[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.lcm(a, b)
    def name                  : String    = "Lcm"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Gcd[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.gcd(a, b)
    def name                  : String    = "Gcd"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class RoundTo[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.roundTo(a, b)
    def name                  : String    = "RoundTo"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class RoundUpTo[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.roundUpTo(a, b)
    def name                  : String    = "RoundUpTo"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Trunc[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.trunc(a, b)
    def name                  : String    = "Trunc"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Atan2[A]()(implicit num: NumDouble[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.atan2(a, b)
    def name                  : String    = "Atan2"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Hypot[A]()(implicit num: NumDouble[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.hypot(a, b)
    def name                  : String    = "Hypot"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Hypotx[A]()(implicit num: NumDouble[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.hypotApx(a, b)
    def name                  : String    = "Hypotx"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Pow[A]()(implicit num: NumDouble[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.pow(a, b)
    def name                  : String    = "Pow"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class LeftShift[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.<<(a, b)
    def name                  : String    = "LeftShift"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class RightShift[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.>>(a, b)
    def name                  : String    = "RightShift"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class UnsignedRightShift[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.>>>(a, b)
    def name                  : String    = "UnsignedRightShift"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  final case class Difsqr[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.difSqr(a, b)
    def name                  : String    = "Difsqr"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Sumsqr[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.sumSqr(a, b)
    def name                  : String    = "Sumsqr"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Sqrsum[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrSum(a, b)
    def name                  : String    = "Sqrsum"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Sqrdif[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.sqrDif(a, b)
    def name                  : String    = "Sqrdif"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Absdif[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.absDif(a, b)
    def name                  : String    = "Absdif"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  //  Thresh
  //  Amclip
  //  Scaleneg

  final case class Clip2[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.clip2(a, b)
    def name                  : String    = "Clip2"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Excess[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.excess(a, b)
    def name                  : String    = "Excess"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Fold2[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.fold2(a, b)
    def name                  : String    = "Fold2"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  final case class Wrap2[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A)     : A         = num.wrap2(a, b)
    def name                  : String    = "Wrap2"
    def aux                   : scala.List[Aux] = num :: Nil
  }

  private final class Expanded[S <: Sys[S], A1, A2, A3, A](op: BinaryOp.Op[A3, A],
                                                           a: ExprLike[S, A1], b: ExprLike[S, A2], tx0: S#Tx)
                                                          (implicit val widen: Widen2[A1, A2, A3])
    extends ExprLike[S, A] with ObservableImpl[S, Change[A]] {

    private[this] val aVal = Ref(a.value(tx0))
    private[this] val bVal = Ref(b.value(tx0))

    private[this] val aObs = a.changed.react { implicit tx => aCh =>
      import TxnLike.peer
      val bBefore = bVal()
      val bNow    = b.value
      val before  = value1(aCh.before, bBefore)
      val now     = value1(aCh.now   , bNow   )
      val ch      = Change(before, now)
      aVal()      = aCh.now
      bVal()      = bNow
      if (ch.isSignificant) fire(ch)
    } (tx0)

    private[this] val bObs = b.changed.react { implicit tx => bCh =>
      import TxnLike.peer
      val aBefore = aVal()
      val aNow    = a.value
      val before  = value1(aBefore, bCh.before)
      val now     = value1(aNow   , bCh.now   )
      val ch      = Change(before, now)
      aVal()      = aNow
      bVal()      = bCh.now
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def changed: Observable[S#Tx, Change[A]] = this

    @inline
    private def value1(av: A1, bv: A2): A = {
      val avw = widen.widen1(av)
      val bvw = widen.widen2(bv)
      op.apply(avw, bvw)
    }

    def value(implicit tx: S#Tx): A = {
      val av = a.value
      val bv = b.value
      value1(av, bv)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      aObs.dispose()
      bObs.dispose()
    }
 }
}
final case class BinaryOp[A1, A2, A3, A](op: BinaryOp.Op[A3, A], a: Ex[A1], b: Ex[A2])
                                        (implicit val widen: Widen2[A1, A2, A3])
  extends Ex[A] { pat =>

  def aux: scala.List[Aux] = widen :: Nil

  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): ExprLike[S, A] = {
    val ax = a.expand[S]
    val bx = b.expand[S]
    new BinaryOp.Expanded[S, A1, A2, A3, A](op, ax, bx, tx)
  }

  //  def value[S <: Base[S]](implicit tx: S#Tx): A = {
//    val av = widen.widen1(a.value[S])
//    val bv = widen.widen2(b.value[S])
//    op.apply(av, bv)
//  }

//  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
//    val aT = t(a)
//    val bT = t(b)
//    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
//  }
}
