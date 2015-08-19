package de.sciss.lucre.stm

import de.sciss.lucre.stm

object InMemory {
  def apply(): InMemory = impl.InMemoryImpl()
}
/** A thin in-memory (non-durable) wrapper around Scala-STM. */
trait InMemory extends InMemoryLike[InMemory] {
  final type Tx = stm.Txn[InMemory]
  final type I  = InMemory
}