/*
 *  SysInMemoryMap.scala
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

package de.sciss.lucre.stm
package impl

import scala.concurrent.stm.TMap

final class SysInMemoryMap[S <: Sys[S], K, V] extends RefMap[S, K, V] {
  private[this] val peer = TMap.empty[K, V]

  def put     (key: K, value: V )(implicit tx: S#Tx): Option[V]  = peer.put      (key, value) (tx.peer)
  def remove  (key: K           )(implicit tx: S#Tx): Option[V]  = peer.remove   (key)        (tx.peer)
  def contains(key: K           )(implicit tx: S#Tx): Boolean    = peer.contains (key)        (tx.peer)
  def get     (key: K           )(implicit tx: S#Tx): Option[V]  = peer.get      (key)        (tx.peer)
  def size                       (implicit tx: S#Tx): Int        = peer.size                  (tx.peer)
  def isEmpty                    (implicit tx: S#Tx): Boolean    = peer.isEmpty               (tx.peer)

  def foreach[B](f: ((K, V)) => B)(implicit tx: S#Tx): Unit = peer.foreach[B](f)(tx.peer)

  def toMap(implicit tx: S#Tx): Map[K, V] = peer.snapshot
}
