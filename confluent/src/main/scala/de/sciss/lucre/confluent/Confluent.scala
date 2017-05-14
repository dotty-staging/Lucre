/*
 *  Confluent.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.confluent

import de.sciss.lucre.confluent.impl.{ConfluentImpl => Impl}
import de.sciss.lucre.{confluent, stm}
import de.sciss.lucre.stm.DataStore

import scala.language.implicitConversions

object Confluent {
  // var DEBUG_DISABLE_PARTIAL = true

  def apply(storeFactory: DataStore.Factory): Confluent = Impl(storeFactory)

  //  trait Txn extends confluent.Txn[Confluent] {
  //    // implicit def durable:  stm.Durable#Tx
  //    // implicit def inMemory: stm.InMemory#Tx
  //  }

  // implicit def inMemory(tx: Confluent#Tx): stm.InMemory#Tx = tx.inMemory
}

trait Confluent extends Sys[Confluent] {
  final protected type S = Confluent
  final type D  = stm.Durable
  final type I  = stm.InMemory
  final type Tx = confluent.Txn[S] // Confluent.Txn
}