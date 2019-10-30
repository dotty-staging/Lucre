/*
 *  QuaternaryOp.scala
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
package graph

import de.sciss.lucre.adjunct.Adjunct
import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.stm.{Base, Sys}

object QuaternaryOp {
  abstract class Op[A, B, C, D, E] extends Product {
    def apply(a: A, b: B, c: C, d: D): E
  }

  abstract class NamedOp[A, B, C, D, E] extends Op[A, B, C, D, E] {
    override def productPrefix = s"QuaternaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  type Adjuncts = scala.List[Adjunct]

  // ---- Seq ----

  final case class SeqMkString[A]() extends NamedOp[Seq[A], String, String, String, String] {
    def apply(a: Seq[A], b: String, c: String, d: String): String =
      a.mkString(b, c, d)

    def name = "SeqMkString"
  }

  final case class SeqPatch[A, B >: A]() extends NamedOp[Seq[A], Int, Seq[B], Int, Seq[B]] {
    def apply(a: Seq[A], from: Int, other: Seq[B], replaced: Int): Seq[B] =
      a.patch(from, other, replaced)

    def name = "SeqPatch"
  }

  private[lucre] final class Expanded[S <: Base[S], A1, A2, A3, A4, A](op: QuaternaryOp.Op[A1, A2, A3, A4, A],
                                                                       a: IExpr[S, A1], b: IExpr[S, A2],
                                                                       c: IExpr[S, A3], d: IExpr[S, A4],
                                                                       tx0: S#Tx)
                                                                  (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IChangeEventImpl[S, A] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)
    c.changed.--->(this)(tx0)
    d.changed.--->(this)(tx0)

    override def toString: String = s"QuaternaryOp($op, $a, $b, $c, $d)"

    def changed: IChangeEvent[S, A] = this

    private[lucre] def pullChange(pull: IPull[S], isNow: Boolean)(implicit tx: S#Tx): A = {
      val _1c = a.changed
      val _2c = b.changed
      val _3c = c.changed
      val _4c = d.changed

      val _1v = if (pull.contains(_1c)) pull.applyChange(_1c, isNow = isNow) else a.value
      val _2v = if (pull.contains(_2c)) pull.applyChange(_2c, isNow = isNow) else b.value
      val _3v = if (pull.contains(_3c)) pull.applyChange(_3c, isNow = isNow) else c.value
      val _4v = if (pull.contains(_4c)) pull.applyChange(_4c, isNow = isNow) else d.value

      value1(_1v, _2v, _3v, _4v)
    }

    @inline
    private def value1(av: A1, bv: A2, cv: A3, dv: A4): A = {
      op.apply(av, bv, cv, dv)
    }

    def value(implicit tx: S#Tx): A = {
      val av = a.value
      val bv = b.value
      val cv = c.value
      val dv = d.value
      value1(av, bv, cv, dv)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
      c.changed -/-> changed
      d.changed -/-> changed
    }
  }
}
final case class QuaternaryOp[A1, A2, A3, A4, A](op: QuaternaryOp.Op[A1, A2, A3, A4, A],
                                                 a: Ex[A1], b: Ex[A2], c: Ex[A3], d: Ex[A4])
  extends Ex[A] {

  type Repr[S <: Sys[S]] = IExpr[S, A]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val ax = a.expand[S]
    val bx = b.expand[S]
    val cx = c.expand[S]
    val dx = d.expand[S]
    new QuaternaryOp.Expanded[S, A1, A2, A3, A4, A](op, ax, bx, cx, dx, tx)
  }
}
