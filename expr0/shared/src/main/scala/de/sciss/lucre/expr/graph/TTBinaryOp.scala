/*
 *  TTBinaryOp.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.{Context, ITrigger}
import de.sciss.lucre.impl.IEventImpl
import de.sciss.lucre.{Exec, IEvent, IPull, ITargets, Txn}

object TTBinaryOp extends ProductReader[TTBinaryOp] {
  sealed trait Op extends Product {
    def apply(a: Boolean, b: Boolean): Boolean

    override final def productPrefix = s"TTBinaryOp$$$name"

    def name: String

    override def toString: String = name
  }

  object And extends ProductReader[And] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): And = {
      require (arity == 0 && adj == 0)
      new And
    }
  }
  final case class And() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a & b

    def name = "And"
  }

  object Or extends ProductReader[Or] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Or = {
      require (arity == 0 && adj == 0)
      new Or
    }
  }
  final case class Or() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a | b

    def name = "Or"
  }

  object Xor extends ProductReader[Xor] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Xor = {
      require (arity == 0 && adj == 0)
      new Xor
    }
  }
  final case class Xor() extends Op {
    def apply(a: Boolean, b: Boolean): Boolean = a ^ b

    def name = "Xor"
  }

  private final class Expanded[T <: Exec[T]](op: Op, a: ITrigger[T], b: ITrigger[T], tx0: T)
                                            (implicit protected val targets: ITargets[T])
    extends ITrigger[T] with IEventImpl[T, Unit] {

    a.changed.--->(this)(tx0)
    b.changed.--->(this)(tx0)

    override def toString: String = s"TTBinaryOp($op, $a, $b)"

    def changed: IEvent[T, Unit] = this

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
      val _1c = a.changed
      val _2c = b.changed

      val at = pull.contains(_1c) && pull(_1c).isDefined
      val bt = pull.contains(_2c) && pull(_2c).isDefined

      val t = op(at, bt)
      if (t) Trig.Some else None
    }

    def dispose()(implicit tx: T): Unit = {
      a.changed -/-> changed
      b.changed -/-> changed
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): TTBinaryOp = {
    require (arity == 3 && adj == 0)
    val _op = in.readProductT[Op]()
    val _a  = in.readTrig()
    val _b  = in.readTrig()
    new TTBinaryOp(_op, _a, _b)
  }
}
final case class TTBinaryOp(op: TTBinaryOp.Op, a: Trig, b: Trig) extends Trig {
  type Repr[T <: Txn[T]] = ITrigger[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val ax = a.expand[T]
    val bx = b.expand[T]
    new TTBinaryOp.Expanded[T](op, ax, bx, tx)
  }
}
