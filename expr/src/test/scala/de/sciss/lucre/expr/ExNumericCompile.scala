package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.expr.ExImport._

trait ExNumericCompile {
  def a: Long
  def b: Ex[Long]

  val c = a - b
}
