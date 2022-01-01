/*
 *  AbstractRand.scala
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

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Caching, IChangeEvent, IExpr, IPull, IPush, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

/** Base for expanded `Rand`, `Rand2`, `RangeRand` */
abstract class AbstractRand[T <: Txn[T], A](zero: A)
  extends IChangeGeneratorEvent[T, A] with IExpr[T, A] with IActionImpl[T] with Caching {

  // ---- abstract ----

  protected def mkNewValue()(implicit tx: T): A

  protected def pullNewValue(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A

  // ---- impl ----

  private[this] val ref = Ref(zero)

  override def value(implicit tx: T): A =
    IPush.tryPull(this).fold(ref())(_.now)

  override private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase) : A =
    if (pull.isOrigin(this)) pull.resolveChange[A]
    else if (phase.isBefore) ref() else {
      val now = pullNewValue(pull)
      ref() = now
      now
    }


  override def changed: IChangeEvent[T, A] = this

  override def executeAction()(implicit tx: T): Unit = {
    val before  = ref()
    val now     = mkNewValue()
    if (before != now) {
      ref() = now
      fire(Change(before, now))
    }
  }
}
