package de.sciss.lucre.stm

object FifthAttempt {
  trait Base {
    type Tx
  }

  trait Comp[S <: Base] {
    val S: S
  }

  trait Root[S <: Base] extends Comp[S]

  trait Sub[S <: Base] extends Root[S] {
    def height(implicit tx: S.Tx): Int
  }

  def treeHeight[S <: Base](r: Root[S])(implicit tx: r.S.Tx): Int = r match {
    case sub: Sub[S] => sub.height
    case _ => -1
  }
}