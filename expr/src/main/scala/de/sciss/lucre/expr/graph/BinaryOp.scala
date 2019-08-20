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

import de.sciss.file._
import de.sciss.lucre.aux.Aux.{Num, NumDouble, NumFrac, NumInt, NumLogic, Ord, Widen2}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change
import de.sciss.span.SpanLike

object BinaryOp {
  abstract class Op[A, B, C] extends Product {
    def apply(a: A, b: B): C
  }
  
  abstract class NamedOp[A, B, C] extends Op[A, B, C] {
    override def productPrefix = s"BinaryOp$$$name"

    def name: String

    override def toString: String = name
  }
  
  type AuxL = scala.List[Aux]

  // ---- (Num, Num) -> Num ----

  final case class Plus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.+(widen.widen1(a), widen.widen2(b))
    
    def name = "Plus"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Minus[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.-(widen.widen1(a), widen.widen2(b))
    
    def name = "Minus"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Times[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.*(widen.widen1(a), widen.widen2(b))
    
    def name = "Times"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  /** Division, _not_ integer division */
  final case class Div[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumFrac[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num./(widen.widen1(a), widen.widen2(b))
    
    def name = "Div"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class ModJ[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.%(widen.widen1(a), widen.widen2(b))
    
    def name = "ModJ"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Mod[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.mod(widen.widen1(a), widen.widen2(b))
    
    def name = "Mod"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  // ---- (Ord, Ord) -> Boolean ----

  /** Equal */
  final case class Eq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B }) 
    extends NamedOp[A, A, B] with ProductWithAux {
    
    def apply(a: A, b: A): B = eq.eq(a, b)
    
    def name = "Eq"
    
    override def aux: AuxL = eq :: Nil
  }

  /** Not equal */
  final case class Neq[A, B]()(implicit eq: Aux.Eq[A] { type Boolean = B}) 
    extends NamedOp[A, A, B] with ProductWithAux {
    
    def apply(a: A, b: A): B = eq.neq(a, b)
    
    def name = "Neq"
    
    override def aux: AuxL = eq :: Nil
  }

  /** Less than */
  final case class Lt[A, B]()(implicit ord: Ord[A] { type Boolean = B }) 
    extends NamedOp[A, A, B] with ProductWithAux {
    
    def apply(a: A, b: A): B = ord.lt(a, b)
    
    def name = "Lt"
    
    override def aux: AuxL = ord :: Nil
  }

  /** Greater than */
  final case class Gt[A, B]()(implicit ord: Ord[A] { type Boolean = B })
    extends NamedOp[A, A, B] with ProductWithAux {
    
    def apply(a: A, b: A): B = ord.gt(a, b)
    
    def name = "Gt"
    
    override def aux: AuxL = ord :: Nil
  }

  /** Less than or equal */
  final case class Leq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) 
    extends NamedOp[A, A, B] with ProductWithAux {
    
    def apply(a: A, b: A): B = ord.leq(a, b)
    
    def name = "Leq"
    
    override def aux: AuxL = ord :: Nil
  }

  /** Greater than or equal */
  final case class Geq[A, B]()(implicit ord: Ord[A] { type Boolean = B }) 
    extends NamedOp[A, A, B] with ProductWithAux {
    
    def apply(a: A, b: A): B = ord.geq(a, b)
    
    def name = "Geq"
    
    override def aux: AuxL = ord :: Nil
  }

  // ---- (Num, Num) -> Num ----

  final case class Min[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.min(widen.widen1(a), widen.widen2(b))
    
    def name = "Min"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Max[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.max(widen.widen1(a), widen.widen2(b))
    
    def name = "Max"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class And[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAux {

    def apply(a: A, b: A): A = num.&(a, b)

    def name = "And"

    override def aux: AuxL = num :: Nil
  }

  final case class Or[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAux {

    def apply(a: A, b: A): A = num.|(a, b)

    def name = "Or"

    override def aux: AuxL = num :: Nil
  }

  final case class Xor[A]()(implicit num: NumLogic[A])
    extends NamedOp[A, A, A] with ProductWithAux {

    def apply(a: A, b: A): A = num.^(a, b)

    def name = "Xor"

    override def aux: AuxL = num :: Nil
  }

  final case class Lcm[A]()(implicit num: NumInt[A]) 
    extends NamedOp[A, A, A] with ProductWithAux {
    
    def apply(a: A, b: A): A = num.lcm(a, b)
    
    def name = "Lcm"
    
    override def aux: AuxL = num :: Nil
  }

  final case class Gcd[A]()(implicit num: NumInt[A]) 
    extends NamedOp[A, A, A] with ProductWithAux {
    
    def apply(a: A, b: A): A = num.gcd(a, b)
    
    def name = "Gcd"
    
    override def aux: AuxL = num :: Nil
  }

  final case class RoundTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.roundTo(widen.widen1(a), widen.widen2(b))
    
    def name = "RoundTo"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class RoundUpTo[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.roundUpTo(widen.widen1(a), widen.widen2(b))
    
    def name = "RoundUpTo"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Trunc[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.trunc(widen.widen1(a), widen.widen2(b))
    
    def name = "Trunc"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Atan2[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.atan2(widen.widen1(a), widen.widen2(b))
    
    def name = "Atan2"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Hypot[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.hypot(widen.widen1(a), widen.widen2(b))
    
    def name = "Hypot"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Hypotx[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.hypotApx(widen.widen1(a), widen.widen2(b))
    
    def name = "Hypotx"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Pow[A, B, C]()(implicit widen: Widen2[A, B, C], num: NumDouble[C])
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.pow(widen.widen1(a), widen.widen2(b))
    
    def name = "Pow"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class LeftShift[A]()(implicit num: NumInt[A]) 
    extends NamedOp[A, A, A] with ProductWithAux {
    
    def apply(a: A, b: A): A = num.<<(a, b)
    
    def name = "LeftShift"
    
    override def aux: AuxL = num :: Nil
  }

  final case class RightShift[A]()(implicit num: NumInt[A])
    extends NamedOp[A, A, A] with ProductWithAux {
    
    def apply(a: A, b: A): A = num.>>(a, b)
    
    def name = "RightShift"
    
    override def aux: AuxL = num :: Nil
  }

  final case class UnsignedRightShift[A]()(implicit num: NumInt[A]) 
    extends NamedOp[A, A, A] with ProductWithAux {
    
    def apply(a: A, b: A): A = num.>>>(a, b)
    
    def name = "UnsignedRightShift"
    
    override def aux: AuxL = num :: Nil
  }

  //  Ring1
  //  Ring2
  //  Ring3
  //  Ring4

  final case class Difsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.difSqr(widen.widen1(a), widen.widen2(b))
    
    def name = "Difsqr"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Sumsqr[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C])
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.sumSqr(widen.widen1(a), widen.widen2(b))
    
    def name = "Sumsqr"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Sqrsum[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.sqrSum(widen.widen1(a), widen.widen2(b))
    
    def name = "Sqrsum"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Sqrdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.sqrDif(widen.widen1(a), widen.widen2(b))
    
    def name = "Sqrdif"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Absdif[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.absDif(widen.widen1(a), widen.widen2(b))
    
    def name = "Absdif"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  //  Thresh
  //  Amclip
  //  Scaleneg

  final case class Clip2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.clip2(widen.widen1(a), widen.widen2(b))
    
    def name = "Clip2"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Excess[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.excess(widen.widen1(a), widen.widen2(b))
    
    def name = "Excess"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Fold2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.fold2(widen.widen1(a), widen.widen2(b))
    
    def name = "Fold2"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  final case class Wrap2[A, B, C]()(implicit widen: Widen2[A, B, C], num: Num[C]) 
    extends NamedOp[A, B, C] with ProductWithAux {
    
    def apply(a: A, b: B): C = num.wrap2(widen.widen1(a), widen.widen2(b))
    
    def name = "Wrap2"
    
    override def aux: AuxL = widen :: num :: Nil
  }

  // ---- Option ----

  final case class OptionContains[A]() extends NamedOp[Option[A], A, Boolean] {
    def apply(a: Option[A], b: A): Boolean = a.contains(b)
    
    def name = "OptionContains"
  }

  final case class OptionGetOrElse[A]() extends NamedOp[Option[A], A, A] {
    def apply(a: Option[A], b: A): A = a.getOrElse(b)
    
    def name = "OptionGetOrElse"
  }

  final case class OptionOrElse[A]() extends NamedOp[Option[A], Option[A], Option[A]] {
    def apply(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)

    def name = "OptionGetOrElse"
  }

  // ---- Seq ----

  final case class SeqApplyOption[A]() extends NamedOp[Seq[A], Int, Option[A]] {
    def apply(a: Seq[A], b: Int): Option[A] = if (b < 0) None else {
      if (a.lengthCompare(b) <= 0) None else Some(a.apply(b))
    }

    def name = "SeqApplyOption"
  }

  final case class SeqConcat[A]() extends NamedOp[Seq[A], Seq[A], Seq[A]] {
    def apply(a: Seq[A], b: Seq[A]): Seq[A] = a ++ b

    def name = "SeqConcat"
  }

  final case class SeqTake[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.take(b)

    def name = "SeqTake"
  }

  final case class SeqDrop[A]() extends NamedOp[Seq[A], Int, Seq[A]] {
    def apply(a: Seq[A], b: Int): Seq[A] = a.drop(b)

    def name = "SeqDrop"
  }

  // ---- String ----

  final case class StringConcat() extends NamedOp[String, String, String] {
    def apply(a: String, b: String): String = a + b

    def name = "StringConcat"
  }

  final case class StringTake() extends NamedOp[String, Int, String] {
    def apply(a: String, b: Int): String = a.take(b)

    def name = "StringTake"
  }

  final case class StringDrop() extends NamedOp[String, Int, String] {
    def apply(a: String, b: Int): String = a.drop(b)

    def name = "StringDrop"
  }

  // ---- SpanLike ----

  final case class SpanLikeClip() extends NamedOp[SpanLike, Long, Long] {
    def apply(a: SpanLike, b: Long): Long = a.clip(b)

    def name = "SpanLikeClip"
  }

  final case class SpanLikeShift() extends NamedOp[SpanLike, Long, SpanLike] {
    def apply(a: SpanLike, b: Long): SpanLike = a.shift(b)

    def name = "SpanLikeShift"
  }

  final case class SpanLikeContains() extends NamedOp[SpanLike, Long, Boolean] {
    def apply(a: SpanLike, b: Long): Boolean = a.contains(b)

    def name = "SpanLikeContains"
  }

  final case class SpanLikeOverlaps() extends NamedOp[SpanLike, SpanLike, Boolean] {
    def apply(a: SpanLike, b: SpanLike): Boolean = a.overlaps(b)

    def name = "SpanLikeOverlaps"
  }

  final case class SpanLikeTouches() extends NamedOp[SpanLike, SpanLike, Boolean] {
    def apply(a: SpanLike, b: SpanLike): Boolean = a.touches(b)

    def name = "SpanLikeTouches"
  }

  final case class SpanLikeUnion() extends NamedOp[SpanLike, SpanLike, SpanLike] {
    def apply(a: SpanLike, b: SpanLike): SpanLike = a.union(b)

    def name = "SpanLikeUnion"
  }

  final case class SpanLikeIntersect() extends NamedOp[SpanLike, SpanLike, SpanLike] {
    def apply(a: SpanLike, b: SpanLike): SpanLike = a.intersect(b)

    def name = "SpanLikeIntersect"
  }

  // ---- File ----

  final case class FileReplaceExt() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a.replaceExt(s)

    def name = "FileReplaceExt"
  }

  final case class FileReplaceName() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a.replaceName(s)

    def name: String = "FileReplaceName"
  }

  final case class FileChild() extends NamedOp[File, String, File] {
    def apply(a: File, s: String): File = a./(s)

    def name: String = "FileChild"
  }

  // ---- Impl ----

  private[lucre] final class Expanded[S <: Base[S], A1, A2, A3, A](op: BinaryOp.Op[A1, A2, A],
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
  extends Ex[A] {

  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val ax = a.expand[S]
    val bx = b.expand[S]
    new BinaryOp.Expanded[S, A1, A2, A3, A](op, ax, bx, tx)
  }
}
