/*
 *  InMemoryIDMapImpl.scala
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

package de.sciss.lucre.confluent
package impl

import de.sciss.lucre.stm.IdentifierMap

import scala.concurrent.stm.TxnLocal

private[impl] final class InMemoryIDMapImpl[S <: Sys[S], A](val store: InMemoryConfluentMap[S, Int])
  extends IdentifierMap[S#ID, S#Tx, A] with InMemoryCacheMapImpl[S, Int] {

  private val markDirtyFlag = TxnLocal(false)

  private def markDirty()(implicit tx: S#Tx): Unit =
    if (!markDirtyFlag.swap(true)(tx.peer)) {
      tx.addDirtyLocalCache(this)
    }

  def get(id: S#ID)(implicit tx: S#Tx): Option[A] =
    getCache[A](id.base, id.path)

  def getOrElse(id: S#ID, default: => A)(implicit tx: S#Tx): A =
    get(id).getOrElse(default)

  def put(id: S#ID, value: A)(implicit tx: S#Tx): Unit = {
    putCache[A](id.base, id.path, value)
    markDirty()
  }

  def contains(id: S#ID)(implicit tx: S#Tx): Boolean =
    get(id).isDefined // XXX TODO more efficient implementation

  def remove(id: S#ID)(implicit tx: S#Tx): Unit =
    if (removeCache(id.base, id.path)) markDirty()

  def dispose()(implicit tx: S#Tx): Unit = ()

  override def toString = s"IdentifierMap@${hashCode().toHexString}"
}