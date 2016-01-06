/*
 *  IdentifierMapImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm.impl

import de.sciss.lucre.stm.{IdentifierMap, TxnLike}
import de.sciss.serial.DataOutput

import scala.concurrent.stm.TMap

object IdentifierMapImpl {
  def newInMemoryIntMap[ID, Tx <: TxnLike, A](implicit intView: ID => Int): IdentifierMap[ID, Tx, A] =
    new InMemoryInt[ID, Tx, A](intView)

  private final class InMemoryInt[ID, Tx <: TxnLike, A](intView: ID => Int)
    extends IdentifierMap[ID, Tx, A] {

    private[this] val peer = TMap.empty[Int, A]

    def get(id: ID)(implicit tx: Tx): Option[A] = peer.get(intView(id))(tx.peer)

    def getOrElse(id: ID, default: => A)(implicit tx: Tx): A = get(id).getOrElse(default)

    def put(id: ID, value: A)(implicit tx: Tx): Unit =
      peer.put(intView(id), value)(tx.peer)

    def contains(id: ID)(implicit tx: Tx): Boolean = peer.contains(intView(id))(tx.peer)

    def remove(id: ID)(implicit tx: Tx): Unit =
      peer.remove(intView(id))(tx.peer)

    override def toString = s"IdentifierMap@${hashCode.toHexString}"

    // def write(out: DataOutput): Unit = ()

    def dispose()(implicit tx: Tx): Unit = ()
  }
}