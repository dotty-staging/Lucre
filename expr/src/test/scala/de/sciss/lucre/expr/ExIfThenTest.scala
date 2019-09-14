package de.sciss.lucre.expr

import de.sciss.lucre.expr.ExImport._
import de.sciss.lucre.expr.graph._
import de.sciss.lucre.stm.{InMemory, UndoManager, Workspace}

object ExIfThenTest extends App {
  type S = InMemory

  val g = Graph {
    val cond    = "cond".attr[Boolean](false)
    val resEx   = If (cond) Then "hello-S" Else "world-S"
    val resAct  = If (cond) Then PrintLn("hello-A") Else PrintLn("world-A")

    LoadBang() ---> PrintLn("cond: " ++ cond.toStr)
    LoadBang() ---> PrintLn(resEx)
    LoadBang() ---> resAct
  }

  implicit val system: S = InMemory()
  implicit val undo: UndoManager[S] = UndoManager()

  import Workspace.Implicits._

  system.step { implicit tx =>
    val self  = IntObj.newConst(0): IntObj[S]
    val selfH = tx.newHandle(self)
    implicit val ctx: Context[S] = Context(Some(selfH))
    self.attr.put("cond", BooleanObj.newConst(true /*false*/))
    g.expand.initControl()
  }
}
