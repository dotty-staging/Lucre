/*
 *  TernaryOp.scala
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

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.aux.Aux.{Num, Widen2}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

object TernaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    def apply(a: A1, b: A1, c: A1): A2

    override final def productPrefix = s"TernaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  // ---- (Num, Num, Num) -> Num ----

  final case class Clip[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A, c: A) : A         = num.clip(a, b, c)
    def name                    : String    = "Clip"
    def aux                     : scala.List[Aux] = num :: Nil
  }

  final case class Fold[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A, c: A) : A         = num.fold(a, b, c)
    def name                    : String    = "Fold"
    def aux                     : scala.List[Aux] = num :: Nil
  }

  final case class Wrap[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A, b: A, c: A) : A         = num.wrap(a, b, c)
    def name                    : String    = "Wrap"
    def aux                     : scala.List[Aux] = num :: Nil
  }

  private final class Expanded[S <: Base[S], A1, A2, A3, A](op: TernaryOp.Op[A3, A],
                                                            a: IExpr[S, A1], b: IExpr[S, A2], c: IExpr[S, A2], tx0: S#Tx)
                                                           (implicit val widen: Widen2[A1, A2, A3],
                                                            protected val targets: ITargets[S])
    extends IExpr[S, A] with IEventImpl[S, Change[A]] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)
    c.changed.--->(this)(tx0)

    override def toString: String = s"TernaryOp($op, $a, $b, $c)"

    def changed: IEvent[S, Change[A]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
      val _1c = a.changed
      val _2c = b.changed
      val _3c = c.changed

      val _1ch = if (pull.contains(_1c)) pull(_1c) else None
      val _2ch = if (pull.contains(_2c)) pull(_2c) else None
      val _3ch = if (pull.contains(_3c)) pull(_3c) else None

      (_1ch, _2ch, _3ch) match {
        case (Some(ach), None, None) =>
          val bv      = b.value
          val cv      = c.value
          val before  = value1(ach.before , bv, cv)
          val now     = value1(ach.now    , bv, cv)
          if (before == now) None else Some(Change(before, now))
        case (None, Some(bch), None) =>
          val av      = a.value
          val cv      = c.value
          val before  = value1(av, bch.before , cv)
          val now     = value1(av, bch.now    , cv)
          if (before == now) None else Some(Change(before, now))
        case (None, None, Some(cch)) =>
          val av      = a.value
          val bv      = b.value
          val before  = value1(av, bv, cch.before )
          val now     = value1(av, bv, cch.now    )
          if (before == now) None else Some(Change(before, now))
        case (Some(ach), Some(bch), None) =>
          val cv      = c.value
          val before  = value1(ach.before , bch.before, cv)
          val now     = value1(ach.now    , bch.now   , cv)
          if (before == now) None else Some(Change(before, now))
        case (None, Some(bch), Some(cch)) =>
          val av      = a.value
          val before  = value1(av, bch.before , cch.before)
          val now     = value1(av, bch.now    , cch.now   )
          if (before == now) None else Some(Change(before, now))
        case (Some(ach), Some(bch), Some(cch)) =>
          val before  = value1(ach.before , bch.before, cch.before)
          val now     = value1(ach.now    , bch.now   , cch.now   )
          if (before == now) None else Some(Change(before, now))
        case _ => None
      }
    }

    @inline
    private def value1(av: A1, bv: A2, cv: A2): A = {
      val avw = widen.widen1(av)
      val bvw = widen.widen2(bv)
      val cvw = widen.widen2(cv)
      op.apply(avw, bvw, cvw)
    }

    def value(implicit tx: S#Tx): A = {
      val av = a.value
      val bv = b.value
      val cv = c.value
      value1(av, bv, cv)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
    }
  }
}
final case class TernaryOp[A1, A2, A3, A](op: TernaryOp.Op[A3, A], a: Ex[A1], b: Ex[A2], c: Ex[A2])
                                        (implicit val widen: Widen2[A1, A2, A3])
  extends Ex.Lazy[A] { pat =>

  def aux: scala.List[Aux] = widen :: Nil

  protected def mkExpr[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] = {
    import ctx.targets
    val ax = a.expand[S]
    val bx = b.expand[S]
    val cx = c.expand[S]
    new TernaryOp.Expanded[S, A1, A2, A3, A](op, ax, bx, cx, tx)
  }
}
