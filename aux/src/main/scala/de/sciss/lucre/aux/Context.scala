package de.sciss.lucre.aux

import de.sciss.lucre.stm.{Base, TxnRandom}

trait Context[S <: Base[S]] {
  /** Creates a new pseudo-random number generator. */
  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: S#Tx): TxnRandom[S]
}
