package de.sciss.lucre.expr

object ExTest {
  def main(args: Array[String]): Unit = {

  }

  def run(): Unit = {
    def Graph[A](fun: => A): A = fun

    import ExOps._

    val g = Graph {

      val x: Ex[Int] = 33
      val y = x * 3
      val z: Ex[Double] = y.ampDb
      z.toInt
    }
  }
}
