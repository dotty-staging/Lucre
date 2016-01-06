/*
 *  InMemory.scala
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

package de.sciss.lucre.stm

object InMemory {
  def apply(): InMemory = impl.InMemoryImpl()
}
/** A thin in-memory (non-durable) wrapper around Scala-STM. */
trait InMemory extends InMemoryLike[InMemory] {
  final type Tx = InMemoryLike.Txn[InMemory]
  final type I  = InMemory
}