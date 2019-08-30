/*
 *  SkipList.scala
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

package de.sciss.lucre.data

import de.sciss.lucre.stm.{Mutable, Base}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object SkipList {
  /** A trait for observing the promotion and demotion of a key
    * in the skip list's level hierarchy
    */
  trait KeyObserver[-Tx, /* @spec(KeySpec) */ -A] {
    /** Notifies the observer that a given key
      * is promoted to a higher (more sparse) level
      */
    def keyUp(key: A)(implicit tx: Tx): Unit

    /** Notifies the observer that a given key
      * is demoted to a lower (more dense) level
      */
    def keyDown(key: A)(implicit tx: Tx): Unit
  }

  // Note: We could also have `object NoKeyObserver extends KeyObserver[ Any, Any ]` if
  // `A` was made contravariant, too. But I guess we would end up in boxing since
  // that wouldn't be specialized any more?
  object NoKeyObserver extends KeyObserver[Any, Any] {
    def keyUp  (key: Any)(implicit tx: Any): Unit = ()
    def keyDown(key: Any)(implicit tx: Any): Unit = ()
  }

  object Set {
    def empty[S <: Base[S], A](implicit tx: S#Tx, ord: Ordering[S#Tx, A],
                              keySerializer: Serializer[S#Tx, S#Acc, A]): SkipList.Set[S, A] =
      HASkipList.Set.empty[S, A]

    def empty[S <: Base[S], A](keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
                             (implicit tx: S#Tx, ord: Ordering[S#Tx, A],
                              keySerializer: Serializer[S#Tx, S#Acc, A]): SkipList.Set[S, A] =
      HASkipList.Set.empty[S, A](keyObserver = keyObserver)

    def read[S <: Base[S], A](in: DataInput, access: S#Acc,
                             keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
                            (implicit tx: S#Tx, ordering: Ordering[S#Tx, A],
                             keySerializer: Serializer[S#Tx, S#Acc, A]): SkipList.Set[S, A] =
      HASkipList.Set.read[S, A](in, access, keyObserver)

    implicit def serializer[S <: Base[S], A](keyObserver: SkipList.KeyObserver[S#Tx, A] = SkipList.NoKeyObserver)
                                           (implicit ordering: Ordering[S#Tx, A],
                                            keySerializer: Serializer[S#Tx, S#Acc, A]): Serializer[S#Tx, S#Acc, Set[S, A]] =
      new SetSer[S, A](keyObserver)

    private final class SetSer[S <: Base[S], A](keyObserver: SkipList.KeyObserver[S#Tx, A])
                                              (implicit ordering: Ordering[S#Tx, A],
                                               keySerializer: Serializer[S#Tx, S#Acc, A])
      extends Serializer[S#Tx, S#Acc, Set[S, A]] {

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Set[S, A] =
        Set.read[S, A](in, access, keyObserver)

      def write(list: Set[S, A], out: DataOutput): Unit = list.write(out)

      override def toString = "SkipList.Set.serializer"
    }
  }

  object Map {
    def empty[S <: Base[S], A, B](implicit tx: S#Tx, ord: Ordering[S#Tx, A],
                                 keySerializer: Serializer[S#Tx, S#Acc, A],
                                 valueSerializer: Serializer[S#Tx, S#Acc, B]): SkipList.Map[S, A, B] =
      HASkipList.Map.empty[S, A, B]

    def empty[S <: Base[S], A, B](keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
                                (implicit tx: S#Tx, ord: Ordering[S#Tx, A],
                                 keySerializer: Serializer[S#Tx, S#Acc, A],
                                 valueSerializer: Serializer[S#Tx, S#Acc, B]): SkipList.Map[S, A, B] =
      HASkipList.Map.empty[S, A, B](keyObserver = keyObserver)

    def read[S <: Base[S], A, B](in: DataInput, access: S#Acc,
                                keyObserver: SkipList.KeyObserver[S#Tx, A] = NoKeyObserver)
                               (implicit tx: S#Tx, ordering: Ordering[S#Tx, A],
                                keySerializer: Serializer[S#Tx, S#Acc, A],
                                valueSerializer: Serializer[S#Tx, S#Acc, B]): SkipList.Map[S, A, B] =
      HASkipList.Map.read[S, A, B](in, access, keyObserver)

    def serializer[S <: Base[S], A, B](keyObserver: SkipList.KeyObserver[S#Tx, A] = SkipList.NoKeyObserver)
                                     (implicit ordering: Ordering[S#Tx, A],
                                      keySerializer: Serializer[S#Tx, S#Acc, A],
                                      valueSerializer: Serializer[S#Tx, S#Acc, B]): Serializer[S#Tx, S#Acc, Map[S, A, B]] =
      new MapSer[S, A, B](keyObserver)

    private final class MapSer[S <: Base[S], A, B](keyObserver: SkipList.KeyObserver[S#Tx, A])
                                                 (implicit ordering: Ordering[S#Tx, A],
                                                  keySerializer: Serializer[S#Tx, S#Acc, A],
                                                  valueSerializer: Serializer[S#Tx, S#Acc, B])
      extends Serializer[S#Tx, S#Acc, Map[S, A, B]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Map[S, A, B] =
        Map.read[S, A, B](in, access, keyObserver)

      def write(list: Map[S, A, B], out: DataOutput): Unit = list.write(out)

      override def toString = "SkipList.Map.serializer"
    }
  }

  trait Set[S <: Base[S], /* @spec(KeySpec) */ A] extends SkipList[S, A, A] {
    /** Inserts a new key into the set.
      *
      * @param   key  the key to insert
      * @return  `true` if the key was new to the set,
      *          `false` if a node with the given key already existed
      */
    def add(key: A)(implicit tx: S#Tx): Boolean

    /** Removes a key from the set.
      *
      * @param key  the key to remove
      * @return     `true` if the key was found and removed, `false` if it was not found
      */
    def remove(key: A)(implicit tx: S#Tx): Boolean
  }

  trait Map[S <: Base[S], /* @spec(KeySpec) */ A, /* @spec(ValueSpec) */ B] extends SkipList[S, A, (A, B)] {

    def keysIterator  (implicit tx: S#Tx): Iterator[A]
    def valuesIterator(implicit tx: S#Tx): Iterator[B]

    /** Inserts a new entry into the map.
      *
      * @param   key    the entry's key to insert
      * @param   value  the entry's value to insert
      * @return  the previous value stored at the key, or `None` if the key was not in the map
      */
    def put(key: A, value: B)(implicit tx: S#Tx): Option[B]

    /** Removes an entry from the map.
      *
      * @param   key  the key to remove
      * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
      */
    def remove(key: A)(implicit tx: S#Tx): Option[B]

    /** Queries the value for a given key.
      *
      * @param key  the key to look for
      * @return     the value if it was found at the key, otherwise `None`
      */
    def get(key: A)(implicit tx: S#Tx): Option[B]

    def getOrElse[B1 >: B](key: A, default: => B1)(implicit tx: S#Tx): B1

    def getOrElseUpdate(key: A, op: => B)(implicit tx: S#Tx): B
  }
}

sealed trait SkipList[S <: Base[S], /* @spec(KeySpec) */ A, /* @spec(ValueSpec) */ E] extends Mutable[S#Id, S#Tx] {
  /** Searches for the Branch of a given key.
    *
    * @param   key   the key to search for
    * @return  `true` if the key is in the list, `false` otherwise
    */
  def contains(key: A)(implicit tx: S#Tx): Boolean

  /** Finds the entry with the largest key which is smaller than or equal to the search key.
    *
    * @param key  the search key
    * @return     the found entry, or `None` if there is no key smaller than or equal
    *             to the search key (e.g. the list is empty)
    */
  def floor(key: A)(implicit tx: S#Tx): Option[E]

  /** Finds the entry with the smallest key which is greater than or equal to the search key.
    *
    * @param key  the search key
    * @return     the found entry, or `None` if there is no key greater than or equal
    *             to the search key (e.g. the list is empty)
    */
  def ceil(key: A)(implicit tx: S#Tx): Option[E]

  /** Returns the first element. Throws an exception if the list is empty. */
  def head(implicit tx: S#Tx): E

  /** Returns the first element, or `None` if the list is empty. */
  def headOption(implicit tx: S#Tx): Option[E]

  /** Returns the last element. Throws an exception if the list is empty. */
  def last(implicit tx: S#Tx): E

  def firstKey(implicit tx: S#Tx): A
  def lastKey (implicit tx: S#Tx): A

  /** Returns the last element, or `None` if the list is empty. */
  def lastOption(implicit tx: S#Tx): Option[E]

  def toIndexedSeq(implicit tx: S#Tx): Vec[E]
  def toList      (implicit tx: S#Tx): List[E]
  def toSeq       (implicit tx: S#Tx): Seq[E]
  def toSet       (implicit tx: S#Tx): Set[E]

  def clear()(implicit tx: S#Tx): Unit

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
  def isomorphicQuery(ord: Ordered[S#Tx, A])(implicit tx: S#Tx): (E, Int)

  // ---- stuff lost from collection.mutable.Set ----

  def +=(entry: E)(implicit tx: S#Tx): this.type
  def -=(key: A)  (implicit tx: S#Tx): this.type

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  def iterator(implicit tx: S#Tx): Iterator[E]

  def debugPrint()(implicit tx: S#Tx): String

  def keySerializer: Serializer[S#Tx, S#Acc, A]

  /** The number of levels in the skip list. */
  def height(implicit tx: S#Tx): Int

  /** Reports the number of keys in the skip list (size of the bottom level).
    * This operation may take up to O(n) time, depending on the implementation.
    */
  def size(implicit tx: S#Tx): Int

  /** The ordering used for the keys of this list. */
  implicit def ordering: Ordering[S#Tx, A]

  /** The minimum gap within elements of each skip level. */
  def minGap: Int

  /** The maximum gap within elements of each skip level. */
  def maxGap: Int
}