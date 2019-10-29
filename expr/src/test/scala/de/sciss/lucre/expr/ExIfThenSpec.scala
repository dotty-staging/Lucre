package de.sciss.lucre.expr

import de.sciss.lucre.stm.{InMemory, UndoManager, Workspace}
import org.scalatest.{FlatSpec, Matchers}

class ExIfThenSpec extends FlatSpec with Matchers with CaptureConsoleOutput {
  type S = InMemory

  def run(condV: Boolean): String = {
    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._

    val g = Graph {
      val cond    = "cond".attr[Boolean](false)
      val resEx   = If (cond) Then         "hello-S"  Else         "world-S"
      val resAct  = If (cond) Then PrintLn("hello-A") Else PrintLn("world-A")

      LoadBang() ---> PrintLn("cond: " ++ cond.toStr)
      LoadBang() ---> PrintLn(resEx)
      LoadBang() ---> resAct
    }

    implicit val system: S = InMemory()
    implicit val undo: UndoManager[S] = UndoManager()

    import Workspace.Implicits._

    captureConsole {
      system.step { implicit tx =>
        val self = IntObj.newConst(0): IntObj[S]
        val selfH = tx.newHandle(self)
        implicit val ctx: Context[S] = Context(Some(selfH))
        self.attr.put("cond", BooleanObj.newConst(condV))
        g.expand.initControl()
      }
    }
  }

  "If-Then-Else" should "work as expected" in {
    val tTrue   = run(condV = true  )
    val tFalse  = run(condV = false )
    val eTrue   =
      """cond: true
        |hello-S
        |hello-A
        |""".stripMargin
    val eFalse   =
      """cond: false
        |world-S
        |world-A
        |""".stripMargin

    assert (tTrue   === eTrue)
    assert (tFalse  === eFalse)
  }
}
