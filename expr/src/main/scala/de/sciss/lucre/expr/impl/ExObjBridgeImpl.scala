/*
 *  ExObjBridgeImpl.scala
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

import de.sciss.lucre.expr.graph.Obj
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{expr, stm}
import de.sciss.serial.{DataOutput, Serializer}

import scala.language.higherKinds

final class ExObjBridgeImpl[A, _Ex[~ <: Sys[~]] <: expr.Expr[~, A]](peer: Type.Expr[A, _Ex])
  extends Obj.Bridge[A] {

  type Repr[S <: Sys[S]] = _Ex[S]

  def mkObj[S <: Sys[S]](value: A)(implicit tx: S#Tx): _Ex[S] =
    peer.newVar(peer.newConst(value))

  def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, _Ex[S]] = peer.serializer

  def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[A]] =
    CellView.attr[S, A, _Ex](map = obj.attr, key = key)(tx, peer)

  def id: Int = Type.Aux.id

  override def write(out: DataOutput): Unit = {
    super.write(out)
    out.writeInt(peer.typeId)
  }
}