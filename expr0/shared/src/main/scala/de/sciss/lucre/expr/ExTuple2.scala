/*
 *  ExTuple2.scala
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

package de.sciss.lucre
package expr

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.impl.IChangeEventImpl

object ExTuple2 extends ProductReader[ExTuple2[_, _]] {
  private[lucre] final class Expanded[T <: Txn[T], T1, T2](val _1: IExpr[T, T1], val _2: IExpr[T, T2])
                                                          (implicit protected val targets: ITargets[T])
    extends IExpr[T, (T1, T2)] with IChangeEventImpl[T, (T1, T2)] {

    def init()(implicit tx: T): this.type = {
      _1.changed ---> changed
      _2.changed ---> changed
      this
    }

    def value(implicit tx: T): (T1, T2) = (_1.value, _2.value)

    def dispose()(implicit tx: T): Unit = {
      _1.changed -/-> changed
      _2.changed -/-> changed
    }

    def changed: IChangeEvent[T, (T1, T2)] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): (T1, T2) = {
      val _1V: T1 = pull.expr(_1)
      val _2V: T2 = pull.expr(_2)
      (_1V, _2V)
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ExTuple2[_, _] = {
    require (arity == 2 && adj == 0)
    val _1 = in.readEx()
    val _2 = in.readEx()
    new ExTuple2(_1, _2)
  }
}
final case class ExTuple2[T1, T2](_1: Ex[T1], _2: Ex[T2]) extends Ex[(T1, T2)] {
  type Repr[T <: Txn[T]] = IExpr[T, (T1, T2)]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    val _1Ex = _1.expand[T]
    val _2Ex = _2.expand[T]
    new ExTuple2.Expanded(_1Ex, _2Ex).init()
  }
}
