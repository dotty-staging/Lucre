/*
 *  ExTuple2.scala
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

import de.sciss.lucre.event.impl.IChangeEventImpl
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.stm.Sys

object ExTuple2 {
  private[lucre] final class Expanded[S <: Sys[S], T1, T2](val _1: IExpr[S, T1], val _2: IExpr[S, T2])
                                                         (implicit protected val targets: ITargets[S])
    extends IExpr[S, (T1, T2)] with IChangeEventImpl[S, (T1, T2)] {

    def init()(implicit tx: S#Tx): this.type = {
      _1.changed ---> changed
      _2.changed ---> changed
      this
    }

    def value(implicit tx: S#Tx): (T1, T2) = (_1.value, _2.value)

    def dispose()(implicit tx: S#Tx): Unit = {
      _1.changed -/-> changed
      _2.changed -/-> changed
    }

    def changed: IChangeEvent[S, (T1, T2)] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): (T1, T2) = {
      val _1V: T1 = pull.expr(_1)
      val _2V: T2 = pull.expr(_2)
      (_1V, _2V)
    }
  }
}
final case class ExTuple2[T1, T2](_1: Ex[T1], _2: Ex[T2]) extends Ex[(T1, T2)] {
  type Repr[S <: Sys[S]] = IExpr[S, (T1, T2)]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    import ctx.targets
    val _1Ex = _1.expand[S]
    val _2Ex = _2.expand[S]
    new ExTuple2.Expanded(_1Ex, _2Ex).init()
  }
}
