/*
 *  BinaryMappedObjIExpr.scala
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

/** Mapping expression on an `Ex[Obj]` and one additional `Ex` argument.
  * Since `Obj.peer` is an option, it must work also in the case that the peer is not present.
  */
abstract class BinaryMappedObjIExpr[T <: Txn[T], P[~ <: Txn[~]] <: LObj[~],
  O <: Obj { type Peer[~ <: Txn[~]] = P[~] }, C, A](in: IExpr[T, O], b: IExpr[T, C], tx0: T)
                                                   (implicit protected val targets: ITargets[T])
  extends IExpr[T, A] with IChangeGeneratorEvent[T, A] with Caching {

  // ---- abstract ----

  protected def observeObj(in: P[T])(implicit tx: T): Disposable[T]

  protected def mapValue(inOpt: Option[P[T]], b: C, isInit: Boolean)(implicit tx: T): A

  // ---- impl ----

  private[this]   val obs = Ref[Disposable[T]](Disposable.empty)
  protected final val ref = Ref.make[A]()

  // ---- init ----
  in.changed.--->(this)(tx0)
  b .changed.--->(this)(tx0)
  setObj(in.value(tx0), b.value(tx0), isInit = true)(tx0)

  final protected def updateFromObj(now: A)(implicit tx: T): Unit = {
    val before = ref.swap(now)
    if (before != now) {
      // println(s"updateFromObj($before, $now)")
      fire(Change(before, now))
    }
  }

  private def setObj(inV: O, bV: C, isInit: Boolean)(implicit tx: T): A = {
    // println(s"setObj($inV, $bV)")
    val peerOpt = inV.peer[T]
    val obsNew  = peerOpt.fold(Disposable.empty[T])(observeObj)
    obs.swap(obsNew).dispose()
    val now = mapValue(peerOpt, bV, isInit = isInit)
    ref()   = now
    now
  }

  def value(implicit tx: T): A =
    IPush.tryPull(this).fold(ref())(_.now)

  final def changed: IChangeEvent[T, A] = this

  def dispose()(implicit tx: T): Unit = {
    in.changed -/-> changed
    b .changed -/-> changed
    obs.swap(Disposable.empty).dispose()
  }

  private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A =
    if (pull.isOrigin(this)) {
      // println(s"pullChange; isOrigin: this, $phase")
      pull.resolveExpr(this)
    } else {
      if (phase.isBefore) ref() else {
        val inCh  = pull.contains(in.changed)
        // println(s"pullChange; inCh? $inCh")
        val inV   = pull.expr(in)
        val bV    = pull.expr(b )
        if (inCh) {
          setObj(inV, bV, isInit = false)
        } else {
          val now = mapValue(inV.peer[T], bV, isInit = false)
          ref()   = now
          now
        }
      }
    }
}
