/*
 *  InMemory.scala
 *  (Lucre)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import de.sciss.lucre.stm

import scala.concurrent.stm.InTxn

object InMemoryLike {
  trait ID[S <: InMemoryLike[S]] extends Identifier[S#Tx] {
    private[stm] def id: Int
  }
}
trait InMemoryLike[S <: InMemoryLike[S]] extends Sys[S] with Cursor[S] {
  final type Var[A]   = stm.Var[S#Tx, A]
  // final type Entry[A] = _Var[S#Tx, A]
  final type ID       = InMemoryLike.ID[S]
  final type Acc      = Unit

  private[stm] def newID(peer: InTxn): S#ID
  def wrap(peer: InTxn) : S#Tx
}

object InMemory {
  def apply(): InMemory = impl.InMemoryImpl()
}
/** A thin in-memory (non-durable) wrapper around Scala-STM. */
trait InMemory extends InMemoryLike[InMemory] {
  final type Tx = stm.Txn[InMemory]
  final type I  = InMemory
}