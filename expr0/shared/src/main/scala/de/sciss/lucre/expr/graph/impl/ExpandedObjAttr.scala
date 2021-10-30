/*
 *  ExpandedObjAttr.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.CellView
import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.expr.graph.Obj.Bridge
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Disposable, IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

// XXX TODO: currently affected by https://github.com/Sciss/Lucre/issues/57
final class ExpandedObjAttr[T <: Txn[T], A](obj: IExpr[T, Obj], key: String, tx0: T)
                                                (implicit protected val targets: ITargets[T], bridge: Bridge[A])
  extends IExpr[T, Option[A]] with IChangeGeneratorEvent[T, Option[A]] {

  private[this] val viewRef   = Ref(Option.empty[CellView.Var[T, Option[A]]])
  private[this] val valueRef  = Ref.make[Option[A]]()
  private[this] val obsRef    = Ref.make[Disposable[T]]()
  // XXX TODO: change for `---> this`; add `Caching` marker trait
  private[this] val objObs    = obj.changed.react { implicit tx => upd =>
    setObj(upd.now, init = false)
  } (tx0)

  private def setNewValue(now: Option[A])(implicit tx: T): Unit = {
    val before = valueRef.swap(now)
    if (before != now) {
      fire(Change(before, now))
    }
  }

  private def setObj(newObj: Obj, init: Boolean)(implicit tx: T): Unit = {
    // println(s"newObj = $newObj, bridge = $bridge, key = $key")
    val newView = newObj.peer[T].map(p => bridge.cellView(p, key))
    viewRef()   = newView
    val obsNew = newView.fold[Disposable[T]](Disposable.empty)(_.react { implicit tx => now =>
      setNewValue(now)
    })
    val now: Option[A] = newView.flatMap(_.apply())
    if (init) {
      obsRef  () = obsNew
      valueRef() = now
    } else {
      obsRef.swap(obsNew).dispose()
      setNewValue(now)
    }
  }

  // ---- init ----
  setObj(obj.value(tx0), init = true)(tx0)

  // XXX TODO handle ongoing IPull properly
  def value(implicit tx: T): Option[A] = viewRef().flatMap(_.apply())

  // XXX TODO if origin is not us but `obj`, handle cache update properly
  private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Option[A] =
    pull.resolveExpr(this)

  def changed: IChangeEvent[T, Option[A]] = this

  def dispose()(implicit tx: T): Unit = {
    objObs  .dispose()
    obsRef().dispose()
  }
}
