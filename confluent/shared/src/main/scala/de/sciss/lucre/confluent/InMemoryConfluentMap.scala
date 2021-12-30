/*
 *  InMemoryConfluentMap.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre.confluent.impl.InMemoryConfluentMapImpl

object InMemoryConfluentMap {
  def newIntMap [T <: Txn[T]]: InMemoryConfluentMap[T, Int]  = new InMemoryConfluentMapImpl[T, Int]
  def newLongMap[T <: Txn[T]]: InMemoryConfluentMap[T, Long] = new InMemoryConfluentMapImpl[T, Long]
}

trait InMemoryConfluentMap[T <: Txn[T], K] {
  def put[A](key: K, value: A, tx: T)(implicit path: tx.Acc): Unit

  /** Finds the most recent value for an entity `id` with respect to version `path`.
    *
    * @param key        the identifier for the object
    * @param path       the path through which the object has been accessed (the version at which it is read)
    * @param tx         the transaction within which the access is performed
    * @tparam A         the type of values stored with the entity
    * @return           `None` if no value was found, otherwise a `Some` of that value.
    */
  def get[A](key: K, tx: T)(implicit path: tx.Acc): Option[A]

  /** Finds the most recent value for an entity `id` with respect to version `path`. If a value is found,
    * it is return along with a suffix suitable for identifier path actualisation.
    *
    * @param key        the identifier for the object
    * @param path       the path through which the object has been accessed (the version at which it is read)
    * @param tx         the transaction within which the access is performed
    * @tparam A         the type of values stored with the entity
    * @return           `None` if no value was found, otherwise a `Some` of the tuple consisting of the
    *                   suffix and the value. The suffix is the access path minus the prefix at which the
    *                   value was found. However, the suffix overlaps the prefix in that it begins with the
    *                   tree entering/exiting tuple at which the value was found.
    */
  def getWithSuffix[A](key: K, tx: T)(implicit path: tx.Acc): Option[(Access[T], A)]

  def remove(key: K, tx: T)(implicit path: tx.Acc): Boolean
}
