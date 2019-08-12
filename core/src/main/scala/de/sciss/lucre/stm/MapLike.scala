/*
 *  MapLike.scala
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

import de.sciss.lucre.event.Observable

import scala.language.higherKinds

object MapLike {
  trait Update[S <: Sys[S], K, Repr[~ <: Sys[~]]] {
    def changes: List[Change[S, K, Repr[S]]]
  }

  sealed trait Change[S <: Sys[S], K, V] {
    def key  : K
    def value: V
  }

  final case class Added   [S <: Sys[S], K, V](key: K, value: V) extends Change[S, K, V]
  final case class Removed [S <: Sys[S], K, V](key: K, value: V) extends Change[S, K, V]
  final case class Replaced[S <: Sys[S], K, V](key: K, before: V, now: V) extends Change[S, K, V] {
    def value: V = now
  }
}
// XXX TODO why scalac does not let us use `Base` (problem in evt.Map)?
trait MapLike[S <: Sys[S], K, Repr[~ <: Sys[~]] /*<: Form[~]*/] extends Disposable[S#Tx] {

  type V = Repr[S]

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  def changed: Observable[S#Tx, MapLike.Update[S, K, Repr]]

//  /** Reports the number of entries in the map.
//    * This operation may take up to O(n) time, depending on the implementation.
//    */
//  def size(implicit tx: S#Tx): Int
//
//  def iterator      (implicit tx: S#Tx): Iterator[(K, V)]
//  def keysIterator  (implicit tx: S#Tx): Iterator[K]
//  def valuesIterator(implicit tx: S#Tx): Iterator[V]

  /** Searches for the map for a given key.
    *
    * @param   key   the key to search for
    * @return  `true` if the key is in the map, `false` otherwise
    */
  def contains(key: K)(implicit tx: S#Tx): Boolean

  /** Queries the value for a given key.
    *
    * @param key  the key to look for
    * @return     the value if it was found at the key, otherwise `None`
    */
  def get(key: K)(implicit tx: S#Tx): Option[V]

//  def $[R[~ <: Sys[~]] <: Repr[~]](key: K)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]]
}