package de.sciss.lucre.event

import de.sciss.lucre.stm.Base

trait IPublisher[S <: Base[S], +A] {
  def changed: IEvent[S, A]
}