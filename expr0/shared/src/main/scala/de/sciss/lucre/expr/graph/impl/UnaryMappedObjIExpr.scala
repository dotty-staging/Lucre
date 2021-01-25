/*
 *  UnaryMappedObjIExpr.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Caching, Disposable, IChangeEvent, IExpr, IPull, IPush, ITargets, Txn, Obj => LObj}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

/** Mapping expression on an `Ex[Obj]`.
  * Since `Obj.peer` is an option, it must work also in the case that the peer is not present.
  */
abstract class UnaryMappedObjIExpr[T <: Txn[T], P[~ <: Txn[~]] <: LObj[~],
  O <: Obj { type Peer[~ <: Txn[~]] = P[~] }, A](in: IExpr[T, O], tx0: T)
                                                (implicit protected val targets: ITargets[T])
  extends IExpr[T, A] with IChangeGeneratorEvent[T, A] with Caching {

  // ---- abstract ----

  /** Create an observer for the peer */
  protected def observeObj(in: P[T])(implicit tx: T): Disposable[T]

  protected def mapValue(inOpt: Option[P[T]], isInit: Boolean)(implicit tx: T): A

  // ---- impl ----

  private[this] val obs = Ref[Disposable[T]](Disposable.empty)
  private[this] val ref = Ref.make[A]()

  in.changed.--->(this)(tx0)
  setObj(in.value(tx0), isInit = true)(tx0)

  //    private def same(a: A, b: A)(implicit tx: T): Boolean =
  //      a == b && {
  //        (a, b) match {
  //          case (ao: Obj, bo: Obj) => ao.peer[T] == bo.peer[T]
  //          case _                  => false
  //        }
  //      }

  final protected def updateFromObj(now: A)(implicit tx: T): Unit = {
    val before = ref.swap(now)
    if (before != now) {
      fire(Change(before, now))
    }
  }

  private def setObj(inV: O, isInit: Boolean)(implicit tx: T): A = {
    val peerOpt = inV.peer[T]
    val obsNew  = peerOpt.fold(Disposable.empty[T])(observeObj)
    obs.swap(obsNew).dispose()
    val now = mapValue(peerOpt, isInit = isInit)
    ref()   = now
    now
  }

  def value(implicit tx: T): A =
    IPush.tryPull(this).fold(ref())(_.now)

  final def changed: IChangeEvent[T, A] = this

  def dispose()(implicit tx: T): Unit = {
    in.changed -/-> changed
    obs.swap(Disposable.empty).dispose()
  }

  private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A =
    if (pull.isOrigin(this)) {
      pull.resolveExpr(this)
    } else {
      if (phase.isBefore) ref() else {
        // val inCh = pull.contains(in.changed)
        val inV = pull.expr(in)
        // if (inCh) {
        setObj(inV, isInit = false)
        // } else {
        //   val now = mapValue(inV.peer[T])
        //   ref()   = now
        //   now
        // }
      }
    }
}
