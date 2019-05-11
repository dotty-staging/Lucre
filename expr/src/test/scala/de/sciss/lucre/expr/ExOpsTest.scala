package de.sciss.lucre.expr

import ExOps._
import de.sciss.lucre.expr.graph.{Act, Ex}

trait ExOpsTest {
  def run(): Unit = {
    def fail(): Nothing = throw new NotImplementedError()

    val x: Ex[Boolean] = fail()

    val tr = x.toTrig

    val y: Ex[Option[Act]] = fail()

    tr ---> y.orNop
  }
}
