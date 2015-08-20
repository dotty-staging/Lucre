/*
 *  ConfluentImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.confluent
package impl

import de.sciss.lucre.event.Observer
import de.sciss.lucre.stm.{IdentifierMap, DataStore, Durable}

import scala.concurrent.stm.InTxn

object ConfluentImpl {
  def apply(storeFactory: DataStore.Factory): Confluent = {
    val mainStore   = storeFactory.open("data")
    val eventStore  = storeFactory.open("event", overwrite = true)
    val durable     = Durable(mainStore, eventStore)
    new System(storeFactory, eventStore, durable)
  }

  private final class System(protected val storeFactory: DataStore.Factory, protected val eventStore: DataStore,
                             val durable: Durable)
    extends Mixin[Confluent] with Confluent {

    def inMemory: I = durable.inMemory

    def durableTx (tx: S#Tx): D#Tx = tx.durable
    def inMemoryTx(tx: S#Tx): I#Tx = tx.inMemory

    protected def wrapRegular(dtx: D#Tx, inputAccess: S#Acc, retroactive: Boolean, cursorCache: Cache[S#Tx]): S#Tx =
      new RegularTxn(this, dtx, inputAccess, retroactive, cursorCache)

    protected def wrapRoot(peer: InTxn): S#Tx = new RootTxn(this, peer)

    protected val eventMap: IdentifierMap[S#ID, S#Tx, Map[Int, List[Observer[S, _]]]] = {
      val map = InMemoryConfluentMap.newIntMap[S]
      new InMemoryIDMapImpl(map)
    }
  }
}