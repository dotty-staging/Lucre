package de.sciss.lucre.expr

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.{BooleanObj, InMemory, IntObj, Workspace}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExIfThenSpec extends AnyFlatSpec with Matchers with CaptureConsoleOutput {
  type S = InMemory
  type T = InMemory.Txn

  def run(condV: Boolean): String = {
    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._

    val g = Graph {
      val cond    = "cond".attr[Boolean](false)
      val resEx   = If (cond) Then         "hello-S"  Else         "world-S"
      val resAct  = If (cond) Then PrintLn("hello-A") Else PrintLn("world-A")

      LoadBang() --> PrintLn("cond: " ++ cond.toStr)
      LoadBang() --> PrintLn(resEx)
      LoadBang() --> resAct
    }

    implicit val system: S = InMemory()
    implicit val undo: UndoManager[T] = UndoManager()

    import Workspace.Implicits._

    captureConsole {
      system.step { implicit tx =>
        val self = IntObj.newConst(0): IntObj[T]
        val selfH = tx.newHandle(self)
        implicit val ctx: Context[T] = Context(Some(selfH))
        self.attr.put("cond", BooleanObj.newConst(condV))
        g.expand.initControl()
      }
    }
  }

  "If-Then-Else" should "work as expected" in {
    val tTrue : String  = run(condV = true  )
    val tFalse: String  = run(condV = false )
    val eTrue : String  =
      """cond: true
        |hello-S
        |hello-A
        |""".stripMargin
    val eFalse   =
      """cond: false
        |world-S
        |world-A
        |""".stripMargin

//    assert (tTrue   === eTrue)
//    assert (tFalse  === eFalse)

    assert (tTrue  == eTrue )   // triple-equals problem with sjs
    assert (tFalse == eFalse)   // triple-equals problem with sjs
  }
}
