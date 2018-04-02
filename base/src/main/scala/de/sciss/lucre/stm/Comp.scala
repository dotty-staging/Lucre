package de.sciss.lucre.stm

trait Comp[S <: Base] {
  val S: S

  final type $ = S.type

  def dispose()(implicit tx: S.Tx): Unit
}
