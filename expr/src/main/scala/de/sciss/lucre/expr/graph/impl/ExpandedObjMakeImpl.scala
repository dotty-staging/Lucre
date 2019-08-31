/*
 *  ExpandedObjMakeImpl.scala
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

import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{Caching, IEvent, IPush, ITargets}
import de.sciss.lucre.expr.impl.ITriggerConsumer
import de.sciss.lucre.expr.{IAction, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change

import scala.concurrent.stm.Ref

/** Building block for expanded object's with Obj.Make functionality. */
abstract class ExpandedObjMakeImpl[S <: Sys[S], A](implicit protected val targets: ITargets[S])
  extends IExpr[S, A]
    with IAction[S]
    with IGenerator       [S, Change[A]]
    with ITriggerConsumer [S, Change[A]]
    with Caching {

  /** Must not be a `val` (used during initialization). */
  protected def empty: A

  protected def make()(implicit tx: S#Tx): A

  private[this] val ref = Ref[A](empty)

  def value(implicit tx: S#Tx): A =
    IPush.tryPull(this).fold(ref())(_.now)

  def executeAction()(implicit tx: S#Tx): Unit =
    trigReceived().foreach(fire)

  protected def trigReceived()(implicit tx: S#Tx): Option[Change[A]] = {
    val now     = make()
    val before  = ref.swap(now) // needs caching
    Some(Change(before, now))
  }

  def changed: IEvent[S, Change[A]] = this
}
