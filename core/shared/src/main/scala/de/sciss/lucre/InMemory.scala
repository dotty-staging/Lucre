/*
 *  InMemory.scala
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

import de.sciss.lucre.impl.InMemoryImpl

object InMemory {
  trait Txn extends InMemoryLike.Txn[Txn] {
    override final type I = Txn
  }

  def apply(): InMemory = InMemoryImpl()
}
/** A thin in-memory (non-durable) wrapper around Scala-STM. */
trait InMemory extends InMemoryLike[InMemory.Txn] {
  //  final type Tx = InMemoryLike.Txn[InMemory]
//  final type I  = InMemory
}