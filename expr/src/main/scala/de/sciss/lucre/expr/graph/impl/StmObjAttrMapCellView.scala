/*
 *  StmObjAttrMapCellView.scala
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

import de.sciss.lucre.expr.CellView
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys}

// XXX TODO --- unfortunate that despite MapLike we have to distinguish
// because stm.Obj.AttrMap must be put into a handle...

/** A `CellView[S#Tx, Option[stm.Obj[S]]` built from an `stm.Obj.attr` */
final class StmObjAttrMapCellView[S <: Sys[S]](attr0: stm.Obj.AttrMap[S], key: String, tx0: S#Tx)
  extends CellView[S#Tx, Option[stm.Obj[S]]] {

  private[this] val attrH = tx0.newHandle(attr0)

  private def attr(implicit tx: S#Tx) = attrH()

  def apply()(implicit tx: S#Tx): Option[stm.Obj[S]] =
    attr.get(key)

  def react(fun: S#Tx => Option[stm.Obj[S]] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case stm.Obj.AttrAdded(`key`, obj) =>
          fun(tx)(Some(obj))

        case stm.Obj.AttrRemoved(`key`, _) =>
          fun(tx)(None)

        case stm.Obj.AttrReplaced(`key`, _, now) =>
          fun(tx)(Some(now))

        case _ => // ignore
      }
    }
  }
}
