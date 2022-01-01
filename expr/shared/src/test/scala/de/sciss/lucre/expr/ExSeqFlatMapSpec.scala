package de.sciss.lucre.expr

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.{InMemory, IntObj, StringObj, Workspace, Folder => LFolder}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExSeqFlatMapSpec extends AnyFlatSpec with Matchers with CaptureConsoleOutput {
  type S = InMemory
  type T = InMemory.Txn

  "Ex[Seq[A]].flatMap" should "work as expected" in {
    val g = Graph {
      import ExImport._
      import graph._
      val f = "folder".attr[Folder](Folder())
      val children = f.children
      val names0 = children.flatMap { obj =>
        obj.attr[String]("name") // .getOrElse("?")
      }
      LoadBang() --> PrintLn(names0.mkString(" "))
    }

    implicit val system: S = InMemory()
    implicit val undo: UndoManager[T] = UndoManager()

    import Workspace.Implicits._

    val res: String = captureConsole {
      system.step { implicit tx =>
        val self = IntObj.newConst(0): IntObj[T]
        val selfH = tx.newHandle(self)
        implicit val ctx: Context[T] = Context(Some(selfH))
        val f   = LFolder[T]()
        def add(name: String): Unit = {
          val c = IntObj.newConst[T](0)
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
    val exp: String =
      """foo bar quux
        |""".stripMargin

//    Console.err.println(s"---- EXP ----\n\n$exp\n\n---- RES ----\n\n$res\n")

//    assert (res === exp)
    assert (res == exp)     // triple-equals problem with sjs
  }
}
