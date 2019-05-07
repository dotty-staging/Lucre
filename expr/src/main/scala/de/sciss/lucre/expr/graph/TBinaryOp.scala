/*
 *  TBinaryOp.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, IExpr, ITrigger}
import de.sciss.lucre.stm.{Base, Sys}

object TBinaryOp {
  sealed trait Op[A] extends Product {
    def apply(a: Boolean, b: A): Boolean

    override final def productPrefix = s"TBinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  final case class And() extends Op[Boolean] {
    def apply(a: Boolean, b: Boolean): Boolean = a & b

    def name = "And"
  }

  private final class Expanded[S <: Base[S], A](op: Op[A], a: ITrigger[S], b: IExpr[S, A], tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S])
    extends ITrigger[S] with IEventImpl[S, Unit] {

    a.changed.--->(this)(tx0)

    override def toString: String = s"TBinaryOp($op, $a, $b)"

    def changed: IEvent[S, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] = {
      val aEvt  = a.changed
      val at    = pull.contains(aEvt) && pull(aEvt).isDefined
      val bt    = b.value

      val t = op(at, bt)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: S#Tx): Unit =
      a.changed -/-> changed
  }
}
final case class TBinaryOp[A](op: TBinaryOp.Op[A], a: Trig, b: Ex[A])
  extends Trig.Lazy {

  protected def mkTrig[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): ITrigger[S] = {
    import ctx.targets
    val ax = a.expand[S]
    val bx = b.expand[S]
    new TBinaryOp.Expanded[S, A](op, ax, bx, tx)
  }
}
