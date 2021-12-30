/*
 *  MapObjLike.scala
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

import de.sciss.lucre.MapObjLike.Update

object MapObjLike {
  trait Update[K, V] {
    def changes: scala.List[Change[K, V]]
  }

  sealed trait Change[K, V] {
    def key  : K
    def value: V
  }

  final case class Added   [K, V](key: K, value: V)           extends Change[K, V]
  final case class Removed [K, V](key: K, value: V)           extends Change[K, V]
  final case class Replaced[K, V](key: K, before: V, now: V)  extends Change[K, V] {
    def value: V = now
  }
}
trait MapObjLike[T <: Exec /*Txn*/[T], K, V] extends Disposable[T] {

//  type V = Repr[T]

  def isEmpty (implicit tx: T): Boolean
  def nonEmpty(implicit tx: T): Boolean

  def changed: Observable[T, Update[K, V]]

  /** Searches for the map for a given key.
   *
   * @param   key   the key to search for
   * @return  `true` if the key is in the map, `false` otherwise
   */
  def contains(key: K)(implicit tx: T): Boolean

  /** Queries the value for a given key.
   *
   * @param key  the key to look for
   * @return     the value if it was found at the key, otherwise `None`
   */
  def get(key: K)(implicit tx: T): Option[V]
}