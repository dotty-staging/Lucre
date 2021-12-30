/*
 *  ObjCellViewVarImpl.scala
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

import de.sciss.lucre.Obj.AttrMap
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.edit.EditAttrMap
import de.sciss.lucre.expr.{CellView, Context}
import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.{Disposable, Source, Txn, Obj => LObj}

import scala.concurrent.stm.Ref
import scala.reflect.ClassTag

abstract class ObjCellViewVarImpl[T <: Txn[T], Dur[~ <: Txn[~]] <: LObj[~],
  In <: Obj { type Peer[~ <: Txn[~]] <: Dur[~] }](
    h: Source[T, LObj[T]], key: String)(implicit ct: ClassTag[Dur[T]], context: Context[T])
  extends CellView.Var[T, Option[In]] {

  // ---- abstract ----

  protected def lower(peer: Dur[T])(implicit tx: T): In

  // ---- impl ----

  type Repr = Option[Dur[T]]

  final def repr(implicit tx: T): Repr =
    h().attr.$[Dur](key)

  protected def putImpl(map: AttrMap[T], value: Dur[T])(implicit tx: T): Unit =
    EditAttrMap.put[T](map, key, value)

  protected def removeImpl(map: AttrMap[T])(implicit tx: T): Unit =
    EditAttrMap.remove[T](map, key)

  final def repr_=(value: Repr)(implicit tx: T): Unit = {
    val a = h().attr
    value match {
      case Some(d)  => putImpl(a, d)
      case None     => removeImpl(a)
    }
  }

  final def lift(value: Option[In])(implicit tx: T): Repr =
    value.flatMap(_.peer[T])

  final def apply()(implicit tx: T): Option[In] = repr.map(lower)

  final def update(v: Option[In])(implicit tx: T): Unit = {
    val peer = v.flatMap(_.peer[T])
    repr = peer
  }

  final def react(fun: T => Option[In] => Unit)(implicit tx: T): Disposable[T] =
    new Observation(h().attr, fun, tx)

  private final class Observation(attr: AttrMap[T], fun: T => Option[In] => Unit,
                                  tx0: T) extends Disposable[T] {
    // XXX TODO --- what was I thinking here? Can we even store an LObj without tx.newHandle here?
    // does not seem to make sense, as comparisons won't work under Confluent system
    private[this] val ref = Ref(Option.empty[Dur[T]])

    // XXX TODO: this was bad. The `Ex[Obj]` should not fire if there are
    // transformations of the underlying durable object. Because anything
    // that might make sense, such as `Folder.size`, will anyway have to be
    // observed directly.
    // private[this] val ref = Ref(Option.empty[(Dur[T], Disposable[T])])
    //
    //private def mkDurObs(d: Dur[T])(implicit tx: T): Disposable[T] =
    //  d.changed.react { implicit tx => _ =>
    //    val ex = lower(d)
    //    fun(tx)(Some(ex))
    //  }

    private def setObj(repr: Repr)(implicit tx: T): Boolean =
      (ref(), repr) match {
        case (None, Some(dNew))  =>
          // val newObs = mkDurObs(dNew)
          // ref() = Some((dNew, newObs))
          ref() = Some(dNew)
          true

        case (Some(_ /*, oldObs */), None) =>
          // oldObs.dispose()
          ref() = None
          true

        // XXX TODO should apply Source instead; this may fire false positives
        case (Some(dOld /*, oldObs */), Some(dNew)) if dOld != dNew =>
          // val newObs = mkDurObs(dNew)
          // ref() = Some((dNew, newObs))
          ref() = Some(dNew)
          // oldObs.dispose()
          true

        case _ => false
      }

    private def setObjAndFire(repr: Repr)(implicit tx: T): Unit =
      if (setObj(repr)) fun(tx)(repr.map(lower))

    private def init()(implicit tx: T): Unit =
      setObj(attr.$[Dur](key))

    init()(tx0)

    private[this] val attrObs = context.reactTo(attr.changed) { implicit tx => upd =>
      upd.changes.foreach {
        case LObj.AttrAdded    (`key`    , d)  => setObjAndFire(ct.unapply(d))
        case LObj.AttrRemoved  (`key`    , _)  => setObjAndFire(None)
        case LObj.AttrReplaced (`key`, _ , d)  => setObjAndFire(ct.unapply(d))
        case _ =>
      }
    } (tx0)

    def dispose()(implicit tx: T): Unit = {
      attrObs.dispose()
      ref.swap(None) // .foreach(_._2.dispose())
    }
  }
}
