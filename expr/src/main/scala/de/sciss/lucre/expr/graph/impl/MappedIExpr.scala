/*
 *  MappedIExpr.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.IExpr
import de.sciss.lucre.stm.Base
import de.sciss.model.Change

abstract class MappedIExpr[S <: Base[S], A1, A](in: IExpr[S, A1], tx0: S#Tx)
                                             (implicit protected val targets: ITargets[S])
  extends IExpr[S, A] with IEventImpl[S, Change[A]] {

  in.changed.--->(this)(tx0)

  protected def mapValue(inValue: A1)(implicit tx: S#Tx): A

  def value(implicit tx: S#Tx): A = mapValue(in.value)

  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
    pull(in.changed).flatMap { ch =>
      val before  = mapValue(ch.before )
      val now     = mapValue(ch.now    )
      if (before == now) None else Some(Change(before, now))
    }

  def dispose()(implicit tx: S#Tx): Unit =
    in.changed.-/->(this)

  def changed: IEvent[S, Change[A]] = this
}
