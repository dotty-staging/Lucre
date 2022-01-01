package de.sciss.lucre.expr

import de.sciss.lucre.expr.ExImport._
import de.sciss.lucre.expr.graph.Ex

trait Issue18 {
  def s1  : Ex[Span]
  def len1: Ex[Long]

  def d1  : Ex[Double]
  def i1  : Ex[Int]

  def insLen: Ex[Long] = s1.start + len1 - 1

  def d2: Ex[Double] = d1 - i1
}
