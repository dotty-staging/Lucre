/*
 *  ITriggerConsumer.scala
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

package de.sciss.lucre.expr.impl

import de.sciss.lucre.event.IPush.Parents
import de.sciss.lucre.event.{IChangePublisher, IPull}
import de.sciss.lucre.expr.ITrigger
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer

import scala.concurrent.stm.Ref

/** A helper implementation for mapping trigger inputs to
  * outputs. Can be used for example with `IAction` and `IGenerator`.
  */
trait ITriggerConsumer[S <: Sys[S], A] extends IChangePublisher[S, A] {
  private[this] val inputs = Ref(List.empty[ITrigger[S]])

  def dispose()(implicit tx: S#Tx): Unit =
    inputs.swap(Nil).foreach { tr =>
      tr.changed -/-> this.changed
    }

  def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit =
    tr.changed ---> this.changed

  protected def valueBefore ()(implicit tx: S#Tx): A
  protected def trigReceived()(implicit tx: S#Tx): A

  private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): A =
    if (pull.isOrigin(this.changed)) pull.resolveChange[A]
    else if (phase.isBefore) valueBefore() else {
      val p: Parents[S] = pull.parents(this.changed)
      if (p.exists(pull(_).isDefined)) trigReceived() else valueBefore()
    }

//  private[lucre] def pullUpdateXXX(pull: IPull[S])(implicit tx: S#Tx) : Option[Change[A]] = {
//    if (pull.isOrigin(this.changed)) Some(pull.resolve)
//    else {
//      val p: Parents[S] = pull.parents(this.changed)
//      if (p.exists(pull(_).isDefined)) trigReceived() else None
//    }
//  }
}
