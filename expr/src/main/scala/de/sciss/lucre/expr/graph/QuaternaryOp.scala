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
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

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
    extends IExpr[S, A] with IEventImpl[S, Change[A]] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)
    c.changed.--->(this)(tx0)
    d.changed.--->(this)(tx0)

    override def toString: String = s"QuaternaryOp($op, $a, $b, $c, $d)"

    def changed: IEvent[S, Change[A]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
      val _1c = a.changed
      val _2c = b.changed
      val _3c = c.changed
      val _4c = d.changed

      val _1Ch0 = if (pull.contains(_1c)) pull(_1c) else None
      val _2Ch0 = if (pull.contains(_2c)) pull(_2c) else None
      val _3Ch0 = if (pull.contains(_3c)) pull(_3c) else None
      val _4Ch0 = if (pull.contains(_4c)) pull(_4c) else None


      val _1Ch = _1Ch0.getOrElse { val v = a.value; Change(v, v) }
      val _2Ch = _2Ch0.getOrElse { val v = b.value; Change(v, v) }
      val _3Ch = _3Ch0.getOrElse { val v = c.value; Change(v, v) }
      val _4Ch = _4Ch0.getOrElse { val v = d.value; Change(v, v) }

      val before  = value1(_1Ch.before , _2Ch.before , _3Ch.before , _4Ch.before)
      val now     = value1(_1Ch.now    , _2Ch.now    , _3Ch.now    , _4Ch.now   )
      val ch      = Change(before, now)
      if (ch.isSignificant) Some(ch) else None
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
