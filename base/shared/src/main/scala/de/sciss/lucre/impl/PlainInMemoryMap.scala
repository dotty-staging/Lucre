/*
 *  PlainInMemoryMap.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package impl

import scala.collection.mutable

final class PlainInMemoryMap[K, V] extends RefMap[Plain, K, V] {
  private[this] val peer = mutable.Map.empty[K, V]

  def put     (key: K, value: V )(implicit tx: Plain): Option[V]  = peer.put      (key, value)
  def remove  (key: K           )(implicit tx: Plain): Option[V]  = peer.remove   (key)
  def contains(key: K           )(implicit tx: Plain): Boolean    = peer.contains (key)
  def get     (key: K           )(implicit tx: Plain): Option[V]  = peer.get      (key)
  def size                       (implicit tx: Plain): Int        = peer.size
  def isEmpty                    (implicit tx: Plain): Boolean    = peer.isEmpty

  def foreach[B](f: ((K, V)) => B)(implicit tx: Plain): Unit = peer.foreach[B](f)

  def toMap(implicit tx: Plain): Map[K, V] = peer.toMap[K, V]
}
