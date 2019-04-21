/*
 *  ExAttrBridgeImpl.scala
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

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.expr
import de.sciss.lucre.expr.graph.Attr
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.DataOutput

import scala.language.higherKinds

final class ExAttrBridgeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](peer: Type.Expr[A, _Ex])
  extends Attr.Bridge[A] {

  def cellView[S <: Sys[S]](obj: Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]] =
    CellView.attr[S, A, _Ex](map = obj.attr, key = key)(tx, peer)

  def id: Int = Type.Aux.id

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(peer.typeId)
  }
}