/*
 *  ObjCellViewVarImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.edit.EditAttrMap
import de.sciss.lucre.expr.CellView
import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}

import scala.concurrent.stm.Ref
import scala.language.higherKinds
import scala.reflect.ClassTag

abstract class ObjCellViewVarImpl[S <: Sys[S], Dur[~ <: Sys[~]] <: stm.Obj[~],
  In <: Obj { type Peer[~ <: Sys[~]] <: Dur[~] }](
    h: stm.Source[S#Tx, stm.Obj[S]], key: String)(implicit ct: ClassTag[Dur[S]])
  extends CellView.Var[S#Tx, Option[In]] {

  // ---- abstract ----

  protected def lower(peer: Dur[S])(implicit tx: S#Tx): In

  // ---- impl ----

  type Repr = Option[Dur[S]]

  final def repr(implicit tx: S#Tx): Repr =
    h().attr.$[Dur](key)

  protected def putImpl(map: AttrMap[S], value: Dur[S])(implicit tx: S#Tx): Unit =
    EditAttrMap.put(map, key, value)

  protected def removeImpl(map: AttrMap[S])(implicit tx: S#Tx): Unit =
    EditAttrMap.remove(map, key)

  final def repr_=(value: Repr)(implicit tx: S#Tx): Unit = {
    val a = h().attr
    value match {
      case Some(d)  => putImpl(a, d)
      case None     => removeImpl(a)
    }
  }

  final def lift(value: Option[In])(implicit tx: S#Tx): Repr =
    value.flatMap(_.peer[S])

  final def apply()(implicit tx: S#Tx): Option[In] = repr.map(lower)

  final def update(v: Option[In])(implicit tx: S#Tx): Unit = {
    val peer = v.flatMap(_.peer)
    repr = peer
  }

  final def react(fun: S#Tx => Option[In] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
    new Observation(h().attr, fun, tx)

  private final class Observation(attr: AttrMap[S], fun: S#Tx => Option[In] => Unit,
                                  tx0: S#Tx) extends Disposable[S#Tx] {
    private[this] val ref = Ref(Option.empty[(Dur[S], Disposable[S#Tx])])

    private def mkDurObs(d: Dur[S])(implicit tx: S#Tx): Disposable[S#Tx] =
      d.changed.react { implicit tx => _ =>
        val ex = lower(d)
        fun(tx)(Some(ex))
      }

    private def setObj(repr: Repr)(implicit tx: S#Tx): Boolean =
      (ref(), repr) match {
        case (None, Some(dNew))  =>
          val newObs = mkDurObs(dNew)
          ref() = Some((dNew, newObs))
          true
        case (Some((_, oldObs)), None) =>
          oldObs.dispose()
          ref() = None
          true
        case (Some((dOld, oldObs)), Some(dNew)) if dOld != dNew =>
          val newObs = mkDurObs(dNew)
          ref() = Some((dNew, newObs))
          oldObs.dispose()
          true
        case _ => false
      }

    private def setObjAndFire(repr: Repr)(implicit tx: S#Tx): Unit =
      if (setObj(repr)) fun(tx)(repr.map(lower))

    private def init()(implicit tx: S#Tx): Unit =
      setObj(attr.$[Dur](key))

    init()(tx0)

    private[this] val attrObs = attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case stm.Obj.AttrAdded    (`key`    , d)  => setObjAndFire(ct.unapply(d))
        case stm.Obj.AttrRemoved  (`key`    , _)  => setObjAndFire(None)
        case stm.Obj.AttrReplaced (`key`, _ , d)  => setObjAndFire(ct.unapply(d))
        case _ =>
      }
    } (tx0)

    def dispose()(implicit tx: S#Tx): Unit = {
      attrObs.dispose()
      ref.swap(None).foreach(_._2.dispose())
    }
  }
}
