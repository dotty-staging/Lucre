package de.sciss.lucre.expr.examples

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.{Context, Graph, graph}
import de.sciss.lucre.{InMemory, Workspace}

/*
  expected output:

  "Henlo"

 */
object LoadBangTest extends App {
  type S = InMemory
  type T = InMemory.Txn

  val g = Graph {
    import graph._

    LoadBang() --> PrintLn("Henlo")
  }

  implicit val system: S = InMemory()
  implicit val undo: UndoManager[T] = UndoManager()

  import Workspace.Implicits._

  implicit val ctx: Context[T] = Context()

  system.step { implicit tx =>
    g.expand.initControl()
  }
}
