package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{Act, Ex}

trait ExOpsTest {
  def run(): Unit = {
    def fail(): Nothing = throw new NotImplementedError()

    val x: Ex[Boolean] = fail()

    val tr = x.toTrig

    val y: Act /*Ex[Option[Act]]*/ = fail()

    tr ---> y // .orNop
  }
}
