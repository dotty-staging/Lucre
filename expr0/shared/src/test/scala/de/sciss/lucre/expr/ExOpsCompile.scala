package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{Act, Ex, PrintLn}

trait ExOpsCompile {
  def run(): Unit = {
    def fail(): Nothing = throw new NotImplementedError()

    val x: Ex[Boolean] = fail()

    val tr = x.toTrig

    val y: Act /*Ex[Option[Act]]*/ = fail()

    val aSq: Ex[Seq[String]]  = fail()
    val bSq: Ex[Seq[String]]  = fail()

    val z   : Ex[Int]       = fail()
    val zSq : Ex[Seq[Int]]  = fail()

    PrintLn(z   .isPowerOfTwo.toStr)
    PrintLn(zSq .isPowerOfTwo.toStr)

    tr.filter(z.isPowerOfTwo)

    (aSq zip bSq).map { case ExTuple2(a, b) =>
      a ++ b
    }

    tr --> y // .orNop
  }
}
