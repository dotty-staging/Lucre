package de.sciss.lucre.stm

/** Any form parametrized in a `Base` system.
  * This trait allows us to pattern match against
  * heterogeneous objects whose only common feature
  * is that they share the system.
  */
trait Form[S]