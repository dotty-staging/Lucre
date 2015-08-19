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
  final type ID       = InMemoryLike.ID[S]
  final type Acc      = Unit

  private[stm] def newID(peer: InTxn): S#ID
  def wrap(peer: InTxn) : S#Tx
}