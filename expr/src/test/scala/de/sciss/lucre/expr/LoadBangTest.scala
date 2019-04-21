package de.sciss.lucre.expr

import de.sciss.lucre.stm.{InMemory, Workspace}

object LoadBangTest extends App {
  type S = InMemory

  val g = Graph {
    import ExOps._
    import graph._

    LoadBang() ---> PrintLn("Henlo")
  }

  implicit val system: S = InMemory()

  import Workspace.Implicits._

  system.step { implicit tx =>
    g.expand[S]().initControl()
  }
}
