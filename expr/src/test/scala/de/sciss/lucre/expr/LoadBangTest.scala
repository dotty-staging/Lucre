package de.sciss.lucre.expr

import de.sciss.lucre.stm.{InMemory, UndoManager, Workspace}

/*
  expected output:

  "Henlo"

 */
object LoadBangTest extends App {
  type S = InMemory

  val g = Graph {
    import graph._

    LoadBang() ---> PrintLn("Henlo")
  }

  implicit val system: S = InMemory()
  implicit val undo: UndoManager[S] = UndoManager()

  import Workspace.Implicits._

  implicit val ctx: Context[S] = Context()

  system.step { implicit tx =>
    g.expand.initControl()
  }
}
