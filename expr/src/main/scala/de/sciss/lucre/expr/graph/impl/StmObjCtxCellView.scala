/*
 *  StmObjCtxCellView.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph.impl

import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.expr.{CellView, Context, ExprLike}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Form, MapLike, Sys}

// XXX TODO --- unfortunate that despite MapLike we have to distinguish
// because stm.Obj.AttrMap must be put into a handle...

abstract class AbstractCtxCellView[S <: Sys[S], A](attr: Context.Attr[S], key: String)
  extends CellView[S#Tx, Option[A]] {

  // ---- abstract ----

  protected def tryParse(value: Any)(implicit tx: S#Tx): Option[A]

  // ---- impl ----

  final def apply()(implicit tx: S#Tx): Option[A] =
    attr.get(key) match {
      case Some(ex: ExprLike[S, _]) => exVal(ex)
      case _                        => None
    }

  @inline
  private def exVal(ex: ExprLike[S, _])(implicit tx: S#Tx): Option[A] =
    tryParse(ex.value)

  private def exValForm(f: Form[S])(implicit tx: S#Tx): Option[A] = f match {
    case ex: ExprLike[S, _] => exVal(ex)
    case _                  => None
  }

  final def react(fun: S#Tx => Option[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case MapLike.Added  (`key`, ex: ExprLike[S, _]) =>
          val opt = exVal(ex)
          if (opt.isDefined) fun(tx)(opt)

        case MapLike.Removed(`key`, ex: ExprLike[S, _]) =>
          val opt = exVal(ex)
          if (opt.isDefined) fun(tx)(None)

        case MapLike.Replaced(`key`, before, now) =>
          val optB = exValForm(before)
          val optN = exValForm(now  )
          if (optB != optN) fun(tx)(optN)

        case _ => // ignore
      }
    }
  }
}

/** A `CellView[S#Tx, Option[stm.Obj[S]]` built from a `Context`. */
final class StmObjCtxCellView[S <: Sys[S]](attr: Context.Attr[S], key: String)
  extends AbstractCtxCellView[S, stm.Obj[S]](attr, key) {

  protected def tryParse(value: Any)(implicit tx: S#Tx): Option[stm.Obj[S]] = value match {
    case obj: Obj => obj.peer
    case _        => None
  }
}
