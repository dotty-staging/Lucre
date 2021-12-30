/*
 *  CacheMap.scala
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

import de.sciss.serial.{ConstFormat, TFormat}

object CacheMap {
  trait InMemory[T <: Txn[T], K, +Store] extends CacheMap[T, K, Store] {
    def putCache[A](key: K, value: A, tx: T)(implicit path: tx.Acc): Unit
    def getCache[A](key: K          , tx: T)(implicit path: tx.Acc): Option[A]
  }

  trait Durable[T <: Txn[T], K, +Store] extends CacheMap[T, K, Store] {
    def putCacheTxn   [A](key: K, value: A, tx: T)(implicit path: tx.Acc, format: TFormat[T, A]): Unit
    def putCacheNonTxn[A](key: K, value: A, tx: T)(implicit path: tx.Acc, format: ConstFormat[A]): Unit

    def getCacheTxn   [A](key: K, tx: T)(implicit path: tx.Acc, format: TFormat[T, A]): Option[A]
    def getCacheNonTxn[A](key: K, tx: T)(implicit path: tx.Acc, format: ConstFormat[A]): Option[A]
  }

  trait Partial[T <: Txn[T], K, +Store] extends CacheMap[T, K, Store] {
    def putPartial[A](key: K, value: A, tx: T)(implicit path: tx.Acc, format: TFormat[T, A]): Unit
    def getPartial[A](key: K          , tx: T)(implicit path: tx.Acc, format: TFormat[T, A]): Option[A]
  }
}

trait CacheMap[T <: Txn[T], K, +Store] extends Cache[T] {
  // ---- abstract ----

  /**
   * The persistent map to which the data is flushed or from which it is retrieved when not residing in cache.
   */
  def store: Store

  // ---- implementation ----

  def getCacheOnly[A] (key: K, tx: T)(implicit path: tx.Acc): Option[A]

  def cacheContains   (key: K, tx: T)(implicit path: tx.Acc): Boolean

  /**
   * Removes an entry from the cache, and only the cache. This will not affect any
   * values also persisted to `persistent`! If the cache does not contain an entry
   * at the given `key`, this method simply returns.
   *
   * @param key        key at which the entry is stored
   * @param tx         the current transaction
   */
  def removeCacheOnly(key: K, tx: T)(implicit path: tx.Acc): Boolean

  /**
   * This method should be invoked from the implementations flush hook after it has
   * determined the terminal version at which the entries in the cache are written
   * to the persistent store. If this method is not called, the cache will just
   * vanish and not be written out to the `persistent` store.
   *
   * @param term    the new version to append to the paths in the cache (using the `PathLike`'s `addTerm` method)
   * @param tx      the current transaction (should be in commit or right-before commit phase)
   */
  def flushCache(term: Long)(implicit tx: T): Unit

  def removeCache(key: K, tx: T)(implicit path: tx.Acc): Boolean
}