/*
 *  Map.scala
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

package de.sciss.lucre.event

import de.sciss.lucre.event.impl.{MapImpl => Impl}
import de.sciss.lucre.stm.{Elem, Obj, Sys}
import de.sciss.serial.{DataInput, ImmutableSerializer, Serializer}

import scala.annotation.switch
import scala.language.higherKinds
import scala.reflect.ClassTag

object Map extends Obj.Type {
  final val typeID = 24

  override def init(): Unit = ()  // this type is known in advance.

  object Key {
    implicit object Int extends Key[Int] {
      final val typeID  = 2 // IntObj.typeID
      def serializer    = ImmutableSerializer.Int // IntObj.valueSerializer
    }
    implicit object Long extends Key[Long] {
      final val typeID  = 3 // LongObj.typeID
      def serializer    = ImmutableSerializer.Long // LongObj.valueSerializer
    }
    implicit object String extends Key[String] {
      final val typeID  = 8 // StringObj.typeID
      def serializer    = ImmutableSerializer.String // StringObj.valueSerializer
    }
    
    def apply(typeID: Int): Key[_] = (typeID: @switch) match {
      case Int   .typeID => Int
      case Long  .typeID => Long
      case String.typeID => String
    }
  }
  /** Cheesy little type class for supported immutable keys. */ 
  sealed trait Key[K] {
    def typeID: Int
    def serializer: ImmutableSerializer[K]
  }

  object Modifiable {
    def apply[S <: Sys[S], K: Key, Repr[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx): Modifiable[S, K, Repr] =
      Impl[S, K, Repr]

    def read[S <: Sys[S], K: Key, Repr[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc)
                                                               (implicit tx: S#Tx): Modifiable[S, K, Repr] =
      serializer[S, K, Repr].read(in, access)

    implicit def serializer[S <: Sys[S], K: Key, Repr[~ <: Sys[~]] <: Elem[~]]: Serializer[S#Tx, S#Acc, Modifiable[S, K, Repr]] =
      Impl.modSerializer[S, K, Repr]
  }

  trait Modifiable[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]] extends Map[S, K, Repr] {
    // override def copy()(implicit tx: S#Tx): Modifiable[S, K, Repr]

    /** Inserts a new entry into the map.
      *
      * @param  key  the key to insert
      * @param  value the value to store for the given key
      * @return the previous value stored at the key, or `None` if the key was not in the map
      */
    def put(key: K, value: V)(implicit tx: S#Tx): Option[V]

    def +=(kv: (K, V))(implicit tx: S#Tx): this.type

    /** Removes an entry from the map.
      *
      * @param   key  the key to remove
      * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
      */
    def remove(key: K)(implicit tx: S#Tx): Option[V]

    def -=(key: K)(implicit tx: S#Tx): this.type
  }

  def read[S <: Sys[S], K: Key, Repr[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc)
                                       (implicit tx: S#Tx): Map[S, K, Repr] =
    serializer[S, K, Repr].read(in, access)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  implicit def serializer[S <: Sys[S], K: Key, Repr[~ <: Sys[~]] <: Elem[~]]: Serializer[S#Tx, S#Acc, Map[S, K, Repr]] =
    Impl.serializer[S, K, Repr]

  final case class Update[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]](map: Map[S, K, Repr],
                                                                        changes: List[Change[S, K, Repr[S]]])

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
trait Map[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Elem[~]]
  extends Obj[S] with Publisher[S, Map.Update[S, K, Repr]] {

  type V = Repr[S]

  def modifiableOption: Option[Map.Modifiable[S, K, Repr]]

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  /** Reports the number of entries in the map.
    * This operation may take up to O(n) time, depending on the implementation.
    */
  def size(implicit tx: S#Tx): Int

  def iterator      (implicit tx: S#Tx): Iterator[(K, V)]
  def keysIterator  (implicit tx: S#Tx): Iterator[K]
  def valuesIterator(implicit tx: S#Tx): Iterator[V]

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

  def $[R[~ <: Sys[~]] <: Repr[~]](key: K)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]]

  // def viewGet(key: K)(implicit tx: S#Tx): stm.Source[S#Tx, Option[V]]
}