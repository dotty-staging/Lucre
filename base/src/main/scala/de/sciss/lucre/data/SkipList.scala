/*
 *  SkipList.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.data

import de.sciss.lucre.stm.{Disposable, Tx}
import de.sciss.serial.Writable

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object SkipList {
  /** A trait for observing the promotion and demotion of a key
    * in the skip list's level hierarchy
    */
  trait KeyObserver[-T, /* @spec(KeySpec) */ -A] {
    /** Notifies the observer that a given key
      * is promoted to a higher (more sparse) level
      */
    def keyUp(key: A)(implicit tx: T): Unit

    /** Notifies the observer that a given key
      * is demoted to a lower (more dense) level
      */
    def keyDown(key: A)(implicit tx: T): Unit
  }

  // Note: We could also have `object NoKeyObserver extends KeyObserver[ Any, Any ]` if
  // `A` was made contravariant, too. But I guess we would end up in boxing since
  // that wouldn't be specialized any more?
  object NoKeyObserver extends KeyObserver[Any, Any] {
    def keyUp  (key: Any)(implicit tx: Any): Unit = ()
    def keyDown(key: Any)(implicit tx: Any): Unit = ()
  }

  object Set {
//    def empty[S <: Sys[S], A](implicit tx: S#Tx, ord: Ordering[S#Tx, A],
//                              keySerializer: Serializer[S#Tx, S#Acc, A]): SkipList.Set[S, A] =
//      HASkipList.Set.empty[S, A]
//
//    def empty[S <: Sys[S], A](keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
//                             (implicit tx: S#Tx, ord: Ordering[S#Tx, A],
//                              keySerializer: Serializer[S#Tx, S#Acc, A]): SkipList.Set[S, A] =
//      HASkipList.Set.empty[S, A](keyObserver = keyObserver)
//
//    def read[S <: Sys[S], A](in: DataInput, access: S#Acc,
//                             keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
//                            (implicit tx: S#Tx, ordering: Ordering[S#Tx, A],
//                             keySerializer: Serializer[S#Tx, S#Acc, A]): SkipList.Set[S, A] =
//      HASkipList.Set.read[S, A](in, access, keyObserver)
//
//    implicit def serializer[S <: Sys[S], A](keyObserver: SkipList.KeyObserver[S#Tx, A] = SkipList.NoKeyObserver)
//                                           (implicit ordering: Ordering[S#Tx, A],
//                                            keySerializer: Serializer[S#Tx, S#Acc, A]): Serializer[S#Tx, S#Acc, Set[S, A]] =
//      new SetSer[S, A](keyObserver)
//
//    private final class SetSer[S <: Sys[S], A](keyObserver: SkipList.KeyObserver[S#Tx, A])
//                                              (implicit ordering: Ordering[S#Tx, A],
//                                               keySerializer: Serializer[S#Tx, S#Acc, A])
//      extends Serializer[S#Tx, S#Acc, Set[S, A]] {
//
//      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Set[S, A] =
//        Set.read[S, A](in, access, keyObserver)
//
//      def write(list: Set[S, A], out: DataOutput): Unit = list.write(out)
//
//      override def toString = "SkipList.Set.serializer"
//    }
  }

  object Map {
//    def empty[S <: Sys[S], A, B](implicit tx: S#Tx, ord: Ordering[S#Tx, A],
//                                 keySerializer: Serializer[S#Tx, S#Acc, A],
//                                 valueSerializer: Serializer[S#Tx, S#Acc, B]): SkipList.Map[S, A, B] =
//      HASkipList.Map.empty[S, A, B]
//
//    def empty[S <: Sys[S], A, B](keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
//                                (implicit tx: S#Tx, ord: Ordering[S#Tx, A],
//                                 keySerializer: Serializer[S#Tx, S#Acc, A],
//                                 valueSerializer: Serializer[S#Tx, S#Acc, B]): SkipList.Map[S, A, B] =
//      HASkipList.Map.empty[S, A, B](keyObserver = keyObserver)
//
//    def read[S <: Sys[S], A, B](in: DataInput, access: S#Acc,
//                                keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
//                               (implicit tx: S#Tx, ordering: Ordering[S#Tx, A],
//                                keySerializer: Serializer[S#Tx, S#Acc, A],
//                                valueSerializer: Serializer[S#Tx, S#Acc, B]): SkipList.Map[S, A, B] =
//      HASkipList.Map.read[S, A, B](in, access, keyObserver)
//
//    def serializer[S <: Sys[S], A, B](keyObserver: SkipList.KeyObserver[S#Tx, A] = SkipList.NoKeyObserver)
//                                     (implicit ordering: Ordering[S#Tx, A],
//                                      keySerializer: Serializer[S#Tx, S#Acc, A],
//                                      valueSerializer: Serializer[S#Tx, S#Acc, B]): Serializer[S#Tx, S#Acc, Map[S, A, B]] =
//      new MapSer[S, A, B](keyObserver)
//
//    private final class MapSer[S <: Sys[S], A, B](keyObserver: SkipList.KeyObserver[S#Tx, A])
//                                                 (implicit ordering: Ordering[S#Tx, A],
//                                                  keySerializer: Serializer[S#Tx, S#Acc, A],
//                                                  valueSerializer: Serializer[S#Tx, S#Acc, B])
//      extends Serializer[S#Tx, S#Acc, Map[S, A, B]] {
//      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Map[S, A, B] =
//        Map.read[S, A, B](in, access, keyObserver)
//
//      def write(list: Map[S, A, B], out: DataOutput): Unit = list.write(out)
//
//      override def toString = "SkipList.Map.serializer"
//    }
  }

  trait Set[T <: Tx, A] extends SkipList[T, A, A] {
    /** Inserts a new key into the set.
      *
      * @param   key  the key to insert
      * @return  `true` if the key was new to the set,
      *          `false` if a node with the given key already existed
      */
    def add(key: A)(implicit tx: T): Boolean

    /** Removes a key from the set.
      *
      * @param key  the key to remove
      * @return     `true` if the key was found and removed, `false` if it was not found
      */
    def remove(key: A)(implicit tx: T): Boolean
  }

  trait Map[T <: Tx, A, B] extends SkipList[T, A, (A, B)] {

    def keysIterator  (implicit tx: T): Iterator[A]
    def valuesIterator(implicit tx: T): Iterator[B]

    /** Inserts a new entry into the map.
      *
      * @param   entry  the key-value pair to insert
      * @return  the previous value stored at the key, or `None` if the key was not in the map
      */
    def add(entry: (A, B))(implicit tx: T): Option[B]

    /** Removes an entry from the map.
      *
      * @param   key  the key to remove
      * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
      */
    def remove(key: A)(implicit tx: T): Option[B]

    /** Queries the value for a given key.
      *
      * @param key  the key to look for
      * @return     the value if it was found at the key, otherwise `None`
      */
    def get(key: A)(implicit tx: T): Option[B]
  }
}

sealed trait SkipList[T <: Tx, A, E] extends Writable with Disposable[T] {
  /** Searches for the Branch of a given key.
    *
    * @param   key   the key to search for
    * @return  `true` if the key is in the list, `false` otherwise
    */
  def contains(key: A)(implicit tx: T): Boolean

  /** Finds the entry with the largest key which is smaller than or equal to the search key.
    *
    * @param key  the search key
    * @return     the found entry, or `None` if there is no key smaller than or equal
    *             to the search key (e.g. the list is empty)
    */
  def floor(key: A)(implicit tx: T): Option[E]

  /** Finds the entry with the smallest key which is greater than or equal to the search key.
    *
    * @param key  the search key
    * @return     the found entry, or `None` if there is no key greater than or equal
    *             to the search key (e.g. the list is empty)
    */
  def ceil(key: A)(implicit tx: T): Option[E]

  def toIndexedSeq(implicit tx: T): Vec[E]
  def toList      (implicit tx: T): List[E]
  def toSeq       (implicit tx: T): Seq[E]
  def toSet       (implicit tx: T): Set[E]

  def clear()(implicit tx: T): Unit

  /** Finds the nearest item equal or greater
    * than an unknown item from an isomorphic
    * set. The isomorphism is represented by
    * a comparison function which guides the
    * binary search.
    *
    * @param   ord   a function that guides the search.
    *                should return -1 if the argument is smaller
    *                than the search key, 0 if both are equivalent,
    *                or 1 if the argument is greater than the search key.
    *                E.g., using some mapping, the function could look
    *                like `mapping.apply(_).compare(queryKey)`
    *
    * @return  the nearest item, or the maximum item
    */
  def isomorphicQuery(ord: Ordered[T, A])(implicit tx: T): (E, Int)

  // ---- stuff lost from collection.mutable.Set ----

  def +=(entry: E)(implicit tx: T): this.type
  def -=(key: A)  (implicit tx: T): this.type

  def isEmpty (implicit tx: T): Boolean
  def nonEmpty(implicit tx: T): Boolean

  def iterator(implicit tx: T): Iterator[E]

  def debugPrint()(implicit tx: T): String

//  def keySerializer: Serializer[T, S#Acc, A]

  /** The number of levels in the skip list. */
  def height(implicit tx: T): Int

  /** Reports the number of keys in the skip list (size of the bottom level).
    * This operation may take up to O(n) time, depending on the implementation.
    */
  def size(implicit tx: T): Int

  /** The ordering used for the keys of this list. */
  implicit def ordering: Ordering[T, A]

  /** The minimum gap within elements of each skip level. */
  def minGap: Int

  /** The maximum gap within elements of each skip level. */
  def maxGap: Int
}