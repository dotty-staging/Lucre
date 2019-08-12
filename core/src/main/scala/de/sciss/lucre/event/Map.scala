/*
 *  Map.scala
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

package de.sciss.lucre.event

import de.sciss.lucre.event.impl.{MapImpl => Impl}
import de.sciss.lucre.stm.{Elem, Form, MapLike, Obj, Sys}
import de.sciss.serial.{DataInput, ImmutableSerializer, Serializer}

import scala.annotation.switch
import scala.language.higherKinds
import scala.reflect.ClassTag

object Map extends Obj.Type {
  final val typeId = 24

  override def init(): Unit = ()  // this type is known in advance.

  object Key {
    implicit object Int extends Key[Int] {
      final val typeId  = 2 // IntObj.typeId
      def serializer: ImmutableSerializer[scala.Int] = Serializer.Int // IntObj.valueSerializer
    }
    implicit object Long extends Key[Long] {
      final val typeId  = 3 // LongObj.typeId
      def serializer: ImmutableSerializer[scala.Long] = Serializer.Long // LongObj.valueSerializer
    }
    implicit object String extends Key[String] {
      final val typeId  = 8 // StringObj.typeId
      def serializer: ImmutableSerializer[java.lang.String] = Serializer.String // StringObj.valueSerializer
    }
    
    def apply(typeId: Int): Key[_] = (typeId: @switch) match {
      case Int   .`typeId` => Int
      case Long  .`typeId` => Long
      case String.`typeId` => String
    }
  }
  /** Cheesy little type class for supported immutable keys. */ 
  sealed trait Key[K] {
    def typeId: Int
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

  trait Modifiable[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Form[~]] extends Map[S, K, Repr] {
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

  final case class Update[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Form[~]](map: Map[S, K, Repr],
                                                                        changes: List[Change[S, K, Repr[S]]])
    extends MapLike.Update[S, K, Repr]

  type Change[S <: Sys[S], K, V] = MapLike.Change[S, K, V]
  
  type Added    [S <: Sys[S], K, V]     = MapLike.Added[S, K, V]
  type Removed  [S <: Sys[S], K, V]     = MapLike.Removed[S, K, V]
  type Replaced [S <: Sys[S], K, V]     = MapLike.Removed[S, K, V]

  val  Added    : MapLike.Added   .type = MapLike.Added
  val  Removed  : MapLike.Removed .type = MapLike.Removed
  val  Replaced : MapLike.Replaced.type = MapLike.Replaced
}
trait Map[S <: Sys[S], K, Repr[~ <: Sys[~]] <: Form[~]]
  extends MapLike[S, K, Repr] with Obj[S] with Publisher[S, Map.Update[S, K, Repr]] {

//  type V = Repr[S]

  def modifiableOption: Option[Map.Modifiable[S, K, Repr]]

  def iterator      (implicit tx: S#Tx): Iterator[(K, V)]
  def keysIterator  (implicit tx: S#Tx): Iterator[K]
  def valuesIterator(implicit tx: S#Tx): Iterator[V]

  def $[R[~ <: Sys[~]] <: Repr[~]](key: K)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]]
}