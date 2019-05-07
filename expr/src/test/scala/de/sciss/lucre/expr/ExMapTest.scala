package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.stm.{InMemory, Workspace}

object ExMapTest extends App {
  type S = InMemory

  val g = Graph {
    import ExOps._
    import graph.{Ex=> _, _}

    val in: Ex[Seq[Double]] = Seq(0.0, 3.0, 6.0)
    val out = in.map(_.dbAmp)
    val b = LoadBang()
    val c = in.changed  // XXX TODO --- can't really test this as `in` is constant
    val t = b | c
    t ---> PrintLn(Const("out[0] = ") ++ out.applyOption(0).toStr)
    t ---> PrintLn(Const("out[1] = ") ++ out.applyOption(1).toStr)
    t ---> PrintLn(Const("out[2] = ") ++ out.applyOption(2).toStr)
  }

  implicit val system: S = InMemory()

  import Workspace.Implicits._

  implicit val ctx: Context[S] = Context()

  system.step { implicit tx =>
    g.expand.initControl()
  }
}
