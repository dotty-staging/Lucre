package de.sciss.lucre.expr

import ExOps._
import de.sciss.lucre.expr.graph.Ex

trait ExOpsTest {
  def run(): Unit = {
    def fail(): Ex[Boolean] = throw new NotImplementedError()

    val x: Ex[Boolean] = fail()

    x.toTrig
  }
}
