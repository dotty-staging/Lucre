/*
 *  Confluent.scala
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

import de.sciss.lucre.confluent.impl.{ConfluentImpl => Impl}

trait ConfluentLike[Tx <: confluent.Txn[Tx]] extends confluent.Sys {
  type T = Tx

//  type D <: DurableLike.Txn[D]
}

object Confluent {
  def apply(storeFactory: DataStore.Factory): Confluent = Impl(storeFactory)
  
  trait Txn extends confluent.Txn[Txn] {
    def system: Confluent

    override type D = Durable  .Txn
    override type I = InMemory .Txn
  }
}

trait Confluent extends ConfluentLike[Confluent.Txn] {
//  final protected type S = Confluent
  final type D = Durable  .Txn
  final type I = InMemory .Txn

  override def durable : Durable
  override def inMemory: InMemory
}