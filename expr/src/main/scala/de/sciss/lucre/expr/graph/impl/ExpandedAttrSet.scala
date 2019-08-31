/*
 *  ExpandedAttrSet.scala
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

import de.sciss.lucre.expr.graph.{Attr, Obj}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{CellView, IExpr}
import de.sciss.lucre.stm.Sys

final class ExpandedAttrSet[S <: Sys[S], A](attrView: CellView.Var[S#Tx, Option[A]], value: IExpr[S, A], tx0: S#Tx)
  extends IActionImpl[S] {

  def executeAction()(implicit tx: S#Tx): Unit = {
    val v = value.value
    attrView.update(Some(v))
  }
}

final class ExpandedAttrSetIn[S <: Sys[S], A](in: IExpr[S, Obj], key: String, value: IExpr[S, A], tx0: S#Tx)
                                             (implicit bridge: Obj.Bridge[A])
  extends IActionImpl[S] {

  def executeAction()(implicit tx: S#Tx): Unit = {
    val v       = value.value
    val obj     = in.value
    val viewOpt = Attr.resolveNestedIn[S, A](obj.peer, key)
    viewOpt.foreach { attrView =>
      attrView.update(Some(v))
    }
  }
}
