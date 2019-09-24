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

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

object ExTuple2 {
  private[lucre] final class Expanded[S <: Sys[S], T1, T2](val _1: IExpr[S, T1], val _2: IExpr[S, T2])
                                                         (implicit protected val targets: ITargets[S])
    extends IExpr[S, (T1, T2)] with IEventImpl[S, Change[(T1, T2)]] {

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

    def changed: IEvent[S, Change[(T1, T2)]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[(T1, T2)]] = {
      val _1Evt = _1.changed
      val _2Evt = _2.changed
      val _1Opt: Option[Change[T1]] = if (pull.contains(_1Evt)) pull(_1Evt) else None
      val _2Opt: Option[Change[T2]] = if (pull.contains(_2Evt)) pull(_2Evt) else None
      val _1Ch = _1Opt.getOrElse {
        val v1 = _1.value
        Change(v1, v1)
      }
      val _2Ch = _2Opt.getOrElse {
        val v2 = _2.value
        Change(v2, v2)
      }
      val ch = Change((_1Ch.before, _2Ch.before), (_1Ch.now, _2Ch.now))
      if (ch.isSignificant) Some(ch) else None
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
