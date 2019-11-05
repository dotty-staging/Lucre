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

import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{Caching, IChangeEvent, IPush, ITargets}
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
    with IChangeGenerator [S, A]
    with ITriggerConsumer [S, A]
    with Caching {

  /** Must not be a `val` (used during initialization). */
  protected def empty: A

  protected def make()(implicit tx: S#Tx): A

  private[this] val ref = Ref[A](empty)

  def value(implicit tx: S#Tx): A =
    IPush.tryPull(this).fold(ref())(_.now)

  def executeAction()(implicit tx: S#Tx): Unit = {
    val ch = Change(valueBefore(), trigReceived())
    if (ch.isSignificant) fire(ch)
  }

  protected def trigReceived()(implicit tx: S#Tx): A = {
    val now = make()
    ref() = now
    now
  }

  protected def valueBefore()(implicit tx: S#Tx): A = ref()

  def changed: IChangeEvent[S, A] = this
}
