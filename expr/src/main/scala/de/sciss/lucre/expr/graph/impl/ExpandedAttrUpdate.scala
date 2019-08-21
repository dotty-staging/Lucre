/*
 *  ExpandedAttrUpdate.scala
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

import de.sciss.lucre.expr.graph.{Attr, Obj}
import de.sciss.lucre.expr.{CellView, IExpr}
import de.sciss.lucre.stm.{Disposable, Sys}

final class ExpandedAttrUpdate[S <: Sys[S], A](source: IExpr[S, A], attrView: CellView.Var[S, Option[A]], tx0: S#Tx)
  extends Disposable[S#Tx] {

  private[this] val obs = source.changed.react { implicit tx => upd =>
    val v = Some(upd.now)
    attrView.update(v)
  } (tx0)

  def dispose()(implicit tx: S#Tx): Unit =
    obs.dispose()
}

final class ExpandedAttrUpdateIn[S <: Sys[S], A](in: IExpr[S, Obj], key: String, value: IExpr[S, A], tx0: S#Tx)
                                                (implicit bridge: Obj.Bridge[A])
  extends Disposable[S#Tx] {

  private[this] val obs = value.changed.react { implicit tx => upd =>
    val v       = Some(upd.now)
    val obj     = in.value
    val viewOpt = Attr.resolveNestedInBAD[S, A](obj.peer, key)
    viewOpt.foreach { attrView =>
      attrView.update(v)
    }
  } (tx0)

  def dispose()(implicit tx: S#Tx): Unit =
    obs.dispose()
}
