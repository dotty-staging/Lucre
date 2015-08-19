package de.sciss.lucre.expr

import de.sciss.lucre.stm.InMemory

object BasicTest {
  def main(args: Array[String]): Unit = {
    val system  = InMemory()
    type S      = InMemory

    IntExtensions    .init()
    BooleanExtensions.init()

    val res = system.step { implicit tx =>
      import IntExtensions.IntExprOps
      val a = Int.newConst[S](1234)
      val b = Int.newConst[S](5678)
      val c = b - a
      c.value
    }

    assert(res == 5678 - 1234)
  }
}