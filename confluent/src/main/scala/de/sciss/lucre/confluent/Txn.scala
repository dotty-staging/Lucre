/*
 *  Txn.scala
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

package de.sciss.lucre.confluent

import de.sciss.lucre.stm
import de.sciss.serial
import de.sciss.serial.{Serializer, ImmutableSerializer}

trait Txn[S <: Sys[S]] extends stm.Txn[S] {
  implicit def durable: S#D#Tx

  def inputAccess: S#Acc

  def info: VersionInfo.Modifiable

  def isRetroactive: Boolean

  /** The confluent handle is enhanced with the `meld` method. */
  def newHandleM[A](value: A)(implicit serializer: Serializer[S#Tx, S#Acc, A]): Source[S, A]

  private[confluent] def readTreeVertexLevel(term: Long): Int
  private[confluent] def addInputVersion(path: S#Acc): Unit

  private[confluent] def putTxn[A](id: S#Id, value: A)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): Unit
  private[confluent] def putNonTxn[A](id: S#Id, value: A)(implicit ser: ImmutableSerializer[A]): Unit

  private[confluent] def getTxn[A](id: S#Id)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): A
  private[confluent] def getNonTxn[A](id: S#Id)(implicit ser: ImmutableSerializer[A]): A

//  private[confluent] def putPartial[A](id: S#Id, value: A)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): Unit
//  private[confluent] def getPartial[A](id: S#Id)(implicit ser: serial.Serializer[S#Tx, S#Acc, A]): A

  private[confluent] def removeFromCache(id: S#Id): Unit

  private[confluent] def addDirtyCache     (cache: Cache[S#Tx]): Unit
  private[confluent] def addDirtyLocalCache(cache: Cache[S#Tx]): Unit

  // private[confluent] def removeDurableIdMap[A](map: stm.IdentifierMap[S#Id, S#Tx, A]): Unit
}