package de.sciss.lucre.expr

import de.sciss.lucre.stm.InMemory

/*
  expected output:
  
  "Change(36,39)"

 */
object ExTest {
  def main(args: Array[String]): Unit =
    run()

  def run(): Unit = {
    import ExOps._

    val a0: Ex[Int] = 22
    val a1: Ex[Int] = 33
    val a = Ex.Var[Int](a0)
    val b = a * 3
    val c: Ex[Double] = b.ampDb
    val g = c.toInt

    type S = InMemory
    val sys: S = InMemory()

    implicit val ctx: Ex.Context[S] = Ex.Context[S]()

    val (obs, ax) = sys.step { implicit tx =>
      val gx  = g.expand[S]
      val _ax = a.expand[S]
      val _obs = gx.changed.react { implicit tx => upd => println(upd) }
      val a1x = a1.expand[S]
      _ax() = a1x
      (_obs, _ax)
    }

    sys.step { implicit tx =>
      obs.dispose()
      ax() = a0.expand[S]
    }
  }
}
