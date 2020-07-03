/*
 *  QuinaryOp.scala
 *  (Lucre)
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
package graph

import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.adjunct.Adjunct.{NumDouble, NumFrac, Widen2}
import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.stm.{Base, Sys}

object QuinaryOp {
  abstract class Op[A, B, C, D, E, F] extends Product {
    def apply(a: A, b: B, c: C, d: D, e: E): F
  }

  abstract class NamedOp[A, B, C, D, E, F] extends Op[A, B, C, D, E, F] {
    override def productPrefix = s"QuinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  type Adjuncts = scala.List[Adjunct]

  // ---- numeric ----

  final case class LinLin[A1, A2, A]()(implicit w: Widen2[A1, A2, A], num: NumFrac[A])
    extends NamedOp[A1, A1, A1, A2, A2, A] with ProductWithAdjuncts {

    def apply(in: A1, inLo: A1, inHi: A1, outLo: A2, outHi: A2): A = {
      val inVal     = w.widen1(in)
      val inLoVal   = w.widen1(inLo)
      val inHiVal   = w.widen1(inHi)
      val outLoVal  = w.widen2(outLo)
      val outHiVal  = w.widen2(outHi)
      num.+(num.*(num./(num.-(inVal, inLoVal), num.-(inHiVal, inLoVal)),
        num.-(outHiVal, outLoVal)), outLoVal)
    }

    def name = "LinLin"

    def adjuncts: List[Adjunct] = w :: num :: Nil
  }

  final case class LinExp[A1, A2, A]()(implicit w: Widen2[A1, A2, A], num: NumDouble[A])
    extends NamedOp[A1, A1, A1, A2, A2, A] with ProductWithAdjuncts {

    def apply(in: A1, inLo: A1, inHi: A1, outLo: A2, outHi: A2): A = {
      val inVal     = w.widen1(in)
      val inLoVal   = w.widen1(inLo)
      val inHiVal   = w.widen1(inHi)
      val outLoVal  = w.widen2(outLo)
      val outHiVal  = w.widen2(outHi)
      num.*(
        num.pow(
          num./(outHiVal, outLoVal),
          num./(num.-(inVal, inLoVal), num.-(inHiVal, inLoVal))),
        outLoVal)
    }

    def name = "LinExp"

    def adjuncts: List[Adjunct] = w :: num :: Nil
  }

  final case class ExpLin[A1, A2, A]()(implicit w: Widen2[A1, A2, A], num: NumDouble[A])
    extends NamedOp[A1, A1, A1, A2, A2, A] with ProductWithAdjuncts {

    def apply(in: A1, inLo: A1, inHi: A1, outLo: A2, outHi: A2): A = {
      val inVal     = w.widen1(in)
      val inLoVal   = w.widen1(inLo)
      val inHiVal   = w.widen1(inHi)
      val outLoVal  = w.widen2(outLo)
      val outHiVal  = w.widen2(outHi)
      num.+(
        num.*(
          num./(
            num.log(num./(inVal  , inLoVal)),
            num.log(num./(inHiVal, inLoVal))),
          num.-(outHiVal, outLoVal)
        ),
        outLoVal
      )
    }

    def name = "ExpLin"

    def adjuncts: List[Adjunct] = w :: num :: Nil
  }

  final case class ExpExp[A1, A2, A]()(implicit w: Widen2[A1, A2, A], num: NumDouble[A])
    extends NamedOp[A1, A1, A1, A2, A2, A] with ProductWithAdjuncts {

    def apply(in: A1, inLo: A1, inHi: A1, outLo: A2, outHi: A2): A = {
      val inVal     = w.widen1(in)
      val inLoVal   = w.widen1(inLo)
      val inHiVal   = w.widen1(inHi)
      val outLoVal  = w.widen2(outLo)
      val outHiVal  = w.widen2(outHi)
      num.*(
        num.pow(
          num./(outHiVal, outLoVal),
          num./(
            num.log(num./(inVal  , inLoVal)),
            num.log(num./(inHiVal, inLoVal))
          )
        ),
        outLoVal
      )
    }

    def name = "ExpExp"

    def adjuncts: List[Adjunct] = w :: num :: Nil
  }

  private[lucre] final class Expanded[S <: Base[S], A1, A2, A3, A4, A5, A](op: QuinaryOp.Op[A1, A2, A3, A4, A5, A],
                                                                       a: IExpr[S, A1], b: IExpr[S, A2],
                                                                       c: IExpr[S, A3], d: IExpr[S, A4],
                                                                       e: IExpr[S, A5],
                                                                       tx0: S#Tx)
                                                                      (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IChangeEventImpl[S, A] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)
    c.changed.--->(this)(tx0)
    d.changed.--->(this)(tx0)
    e.changed.--->(this)(tx0)

    override def toString: String = s"QuinaryOp($op, $a, $b, $c, $d, $e)"

    def changed: IChangeEvent[S, A] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): A = {
      val _1v = pull.expr(a)
      val _2v = pull.expr(b)
      val _3v = pull.expr(c)
      val _4v = pull.expr(d)
      val _5v = pull.expr(e)

      value1(_1v, _2v, _3v, _4v, _5v)
    }

    @inline
    private def value1(av: A1, bv: A2, cv: A3, dv: A4, ev: A5): A = {
      op.apply(av, bv, cv, dv, ev)
    }

    def value(implicit tx: S#Tx): A = {
      val av = a.value
      val bv = b.value
      val cv = c.value
      val dv = d.value
      val ev = e.value
      value1(av, bv, cv, dv, ev)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
      c.changed -/-> changed
      d.changed -/-> changed
      e.changed -/-> changed
    }
  }
}
final case class QuinaryOp[A1, A2, A3, A4, A5, A](op: QuinaryOp.Op[A1, A2, A3, A4, A5, A],
                                                 a: Ex[A1], b: Ex[A2], c: Ex[A3], d: Ex[A4], e: Ex[A5])
  extends Ex[A] {

  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val ax = a.expand[S]
    val bx = b.expand[S]
    val cx = c.expand[S]
    val dx = d.expand[S]
    val ex = e.expand[S]
    new QuinaryOp.Expanded[S, A1, A2, A3, A4, A5, A](op, ax, bx, cx, dx, ex, tx)
  }
}
