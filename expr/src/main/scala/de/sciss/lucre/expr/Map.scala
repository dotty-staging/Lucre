/*
 *  Map.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.impl.{MapImpl => Impl}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.data
import de.sciss.serial.{DataInput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object Map extends Obj.Type {
  final val typeID = 24

  object Modifiable {
    def apply[S <: Sys[S], K, V <: Obj[S]](implicit tx: S#Tx, keyType: Type.Expr[K]): Modifiable[S, K, V] =
      Impl[S, K, V]

    def read[S <: Sys[S], K, V <: Obj[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx, keyType: Type.Expr[K]): Modifiable[S, K, V] =
      Impl.modRead(in, access)

    implicit def serializer[S <: Sys[S], K, V <: Obj[S]](implicit keyType: Type.Expr[K]): Serializer[S#Tx, S#Acc, Modifiable[S, K, V]] =
      Impl.modSerializer
  }

  trait Modifiable[S <: Sys[S], K, V] extends Map[S, K, V] {
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

  def read[S <: Sys[S], K, V <: Obj[S]](in: DataInput, access: S#Acc)
                                       (implicit tx: S#Tx, keyType: Type.Expr[K]): Map[S, K, V] =
    Impl.read(in, access)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  implicit def serializer[S <: Sys[S], K, V <: Obj[S]](implicit keyType: Type.Expr[K]): Serializer[S#Tx, S#Acc, Map[S, K, V]] =
    Impl.serializer

  final case class Update[S <: Sys[S], K, V](map: Map[S, K, V], changes: Vec[Change[S, K, V]])

  sealed trait Change[S <: Sys[S], K, V] {
    def key  : K
    def value: V
  }

  final case class Added  [S <: Sys[S], K, V](key: K, value: V) extends Change[S, K, V]
  final case class Removed[S <: Sys[S], K, V](key: K, value: V) extends Change[S, K, V]
}
trait Map[S <: Sys[S], K, V] extends Obj[S] with Publisher[S, Map.Update[S, K, V]] {
  def modifiableOption: Option[Map.Modifiable[S, K, V]]

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  /** Reports the number of entries in the map.
    * This operation may take up to O(n) time, depending on the implementation.
    */
  def size(implicit tx: S#Tx): Int

  def iterator      (implicit tx: S#Tx): data.Iterator[S#Tx, (K, V)]
  def keysIterator  (implicit tx: S#Tx): data.Iterator[S#Tx, K]
  def valuesIterator(implicit tx: S#Tx): data.Iterator[S#Tx, V]

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

  // def viewGet(key: K)(implicit tx: S#Tx): stm.Source[S#Tx, Option[V]]
}