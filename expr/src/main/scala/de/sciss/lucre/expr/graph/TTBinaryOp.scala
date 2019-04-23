/*
 *  TTBinaryOp.scala
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
import de.sciss.lucre.expr.{Ex, ITrigger, Trig}
import de.sciss.lucre.stm.{Base, Sys}

object TTBinaryOp {
  sealed trait Op extends Product {
    def apply(a: Boolean, b: Boolean): Boolean

    override final def productPrefix = s"TTBinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  final case class And() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a & b

    def name = "And"
  }

  final case class Or() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a | b

    def name = "Or"
  }

  final case class Xor() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a ^ b

    def name = "Xor"
  }

  private final class Expanded[S <: Base[S]](op: Op, a: ITrigger[S], b: ITrigger[S], tx0: S#Tx)
                                            (implicit protected val targets: ITargets[S])
    extends ITrigger[S] with IEventImpl[S, Unit] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)

    override def toString: String = s"TTBinaryOp($op, $a, $b)"

    def changed: IEvent[S, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] = {
      val _1c = a.changed
      val _2c = b.changed

      val at = pull.contains(_1c) && pull(_1c).isDefined
      val bt = pull.contains(_2c) && pull(_2c).isDefined

      val t = op(at, bt)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
    }
  }
}
final case class TTBinaryOp(op: TTBinaryOp.Op, a: Trig, b: Trig)
  extends Trig.Lazy {

  protected def mkTrig[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): ITrigger[S] = {
    import ctx.targets
    val ax = a.expand[S]
    val bx = b.expand[S]
    new TTBinaryOp.Expanded[S](op, ax, bx, tx)
  }
}
