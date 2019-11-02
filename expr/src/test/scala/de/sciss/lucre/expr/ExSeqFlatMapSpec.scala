package de.sciss.lucre.expr

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{InMemory, UndoManager, Workspace}
import org.scalatest.{FlatSpec, Matchers}

class ExSeqFlatMapSpec extends FlatSpec with Matchers with CaptureConsoleOutput {
  type S = InMemory

  "Ex[Seq[A]].flatMap" should "work as expected" in {
    val g = Graph {
      import ExImport._
      import graph._
      val f = "folder".attr[Folder](Folder())
      val children = f.children
      val names0 = children.flatMap { obj =>
        obj.attr[String]("name") // .getOrElse("?")
      }
      LoadBang() ---> PrintLn(names0.mkString(" "))
    }

    implicit val system: S = InMemory()
    implicit val undo: UndoManager[S] = UndoManager()

    import Workspace.Implicits._

    val res = captureConsole {
      system.step { implicit tx =>
        val self = IntObj.newConst(0): IntObj[S]
        val selfH = tx.newHandle(self)
        implicit val ctx: Context[S] = Context(Some(selfH))
        val f   = stm.Folder[S]
        def add(name: String): Unit = {
          val c = IntObj.newConst[S](0)
          c.attr.put("name", StringObj.newConst(name))
          f.addLast(c)
        }
        add("foo")
        add("bar")
        add("quux")

        self.attr.put("folder", f)
        g.expand.initControl()
      }
    }
    val exp =
      """foo bar quux
        |""".stripMargin

    //    Console.err.println(s"---- EXP ----\n\n$exp\n\n---- RES ----\n\n$res\n")

    assert (res === exp)
  }
}
