package de.sciss.lucre.expr.examples

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.ExImport._
import de.sciss.lucre.expr.graph._
import de.sciss.lucre.expr.{Context, Graph}
import de.sciss.lucre.{InMemory, IntObj, Workspace}

/*
  expected output:

  Some(66)

 */
object ExMapTest2 extends App {
  type S = InMemory
  type T = InMemory.Txn

//  de.sciss.lucre.event.showLog = true

  val g = Graph {
    val fAttr = "foo".attr[Int]
    val m = fAttr.map(_ * 2)
    m.changed --> PrintLn(m.toStr)
  }

  implicit val system: S = InMemory()
  implicit val undo: UndoManager[T] = UndoManager()

  import Workspace.Implicits._

  system.step { implicit tx =>
    val self  = IntObj.newConst(0): IntObj[T]
    val selfH = tx.newHandle(self)
    implicit val ctx: Context[T] = Context(Some(selfH))
    g.expand.initControl()
    self.attr.put("foo", IntObj.newConst(33))
  }
}
