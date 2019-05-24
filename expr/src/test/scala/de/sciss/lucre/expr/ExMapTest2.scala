package de.sciss.lucre.expr

import de.sciss.lucre.expr.ExImport._
import de.sciss.lucre.expr.graph._
import de.sciss.lucre.stm.{InMemory, UndoManager, Workspace}

/*
  expected output:

  Some(66)

 */
object ExMapTest2 extends App {
  type S = InMemory

  val g = Graph {
    val fAttr = "foo".attr[Int]
    val m = fAttr.map(_ * 2)
    m.changed ---> PrintLn(m.toStr)
  }

  implicit val system: S = InMemory()
  implicit val undo: UndoManager[S] = UndoManager()

  import Workspace.Implicits._

  system.step { implicit tx =>
    val self  = IntObj.newConst(0): IntObj[S]
    val selfH = tx.newHandle(self)
    implicit val ctx: Context[S] = Context(Some(selfH))
    g.expand.initControl()
    self.attr.put("foo", IntObj.newConst(33))
  }
}
