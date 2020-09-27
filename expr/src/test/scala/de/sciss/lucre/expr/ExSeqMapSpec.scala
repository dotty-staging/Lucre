package de.sciss.lucre.expr

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.{DoubleObj, DoubleVector, InMemory, IntObj, IntVector, Workspace}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExSeqMapSpec extends AnyFlatSpec with Matchers with CaptureConsoleOutput {
  type S = InMemory
  type T = InMemory.Txn

  "Ex[Seq[A]].map" should "work as expected" in {
    val g = Graph {
      import ExImport._
      import graph._

      val in: Ex[Seq[Double]] = "in".attr(Seq.empty[Double])
      val out = in.map(_.dbAmp.roundTo(0.01))
      val b   = LoadBang()
      val c   = out /*in*/.changed
      val t   = b | c
      t ---> PrintLn(Const("out = ") ++ out.mkString(", "))
      //    t ---> PrintLn(Const("out[0] = ") ++ out.applyOption(0).toStr)
      //    t ---> PrintLn(Const("out[1] = ") ++ out.applyOption(1).toStr)
      //    t ---> PrintLn(Const("out[2] = ") ++ out.applyOption(2).toStr)
    }

    implicit val system: S = InMemory()
    implicit val undo: UndoManager[T] = UndoManager()

    import Workspace.Implicits._

    val res = captureConsole {
      system.step { implicit tx =>
        val self = IntObj.newConst(0): IntObj[T]
        val selfH = tx.newHandle(self)
        implicit val ctx: Context[T] = Context(Some(selfH))
        self.attr.put("in", DoubleVector.newConst(Vector(0.0, 3.0, 6.0)))
        g.expand.initControl()
        self.attr.put("in", DoubleVector.newConst(Vector(-3.0, 0.0)))
      }
    }
    val exp =
      """out = 1.0, 1.41, 2.0
        |out = 0.71, 1.0
        |""".stripMargin

    assert (res === exp)
  }

  it should "correctly observe expressions from the closure" in {
    val g = Graph {
      import ExImport._
      import graph._

      val in1: Ex[Seq[Double]]  = "in1".attr(Seq.empty[Double])
      val in2: Ex[Double]       = "in2".attr(0.0)
      val out = in1.map(_ + in2)
      val b   = LoadBang()
      val c   = out.changed
      val t   = b | c
      t ---> PrintLn(Const("out = ") ++ out.mkString(", "))
      //    t ---> PrintLn(Const("out[0] = ") ++ out.applyOption(0).toStr)
      //    t ---> PrintLn(Const("out[1] = ") ++ out.applyOption(1).toStr)
      //    t ---> PrintLn(Const("out[2] = ") ++ out.applyOption(2).toStr)
    }

    implicit val system: S = InMemory()
    implicit val undo: UndoManager[T] = UndoManager()

    import Workspace.Implicits._

    val res = captureConsole {
      system.step { implicit tx =>
        val self = IntObj.newConst(0): IntObj[T]
        val selfH = tx.newHandle(self)
        implicit val ctx: Context[T] = Context(Some(selfH))
        val vr1 = DoubleVector.newVar[T](Vector(0.0, 3.0, 6.0))
        val vr2 = DoubleObj   .newVar[T](0.0)
        self.attr.put("in1", vr1)
        self.attr.put("in2", vr2)
        g.expand.initControl()
        vr1() = Vector(1.0, 2.0, 3.0)
//        de.sciss.lucre.event.showLog = true
        vr2() = 10.0
//        de.sciss.lucre.event.showLog = false
        vr1() = Vector(-4.0, -5.0)
        vr2() = 12.0
      }
    }
    val exp =
      """out = 0.0, 3.0, 6.0
        |out = 1.0, 2.0, 3.0
        |out = 11.0, 12.0, 13.0
        |out = 6.0, 5.0
        |out = 8.0, 7.0
        |""".stripMargin

//    Console.err.println(s"---- EXP ----\n\n$exp\n\n---- RES ----\n\n$res\n")

    assert (res === exp)
  }

  it should "correctly observe more expressions from the closure" in {
    val g = Graph {
      import ExImport._
      import graph._

      val in: Ex[Seq[Int]] = "in".attr(Seq.empty[Int])
      val out = in.map(_ + in.size)
      val b   = LoadBang()
      val c   = out.changed
      val t   = b | c
      t ---> PrintLn(out.mkString("out = [", ", ", "]"))
    }

    implicit val system: S = InMemory()
    implicit val undo: UndoManager[T] = UndoManager()

    import Workspace.Implicits._

    val res = captureConsole {
      system.step { implicit tx =>
        val self = IntObj.newConst(0): IntObj[T]
        val selfH = tx.newHandle(self)
        implicit val ctx: Context[T] = Context(Some(selfH))
        val vr = IntVector.newVar[T](Vector(3, 5, 8))
        self.attr.put("in", vr)
        g.expand.initControl()
        vr() = Vector(13, 21)
        vr() = Vector(13, 20) // ensures that we don't cache first value only
        vr() = Vector()
      }
    }
    val exp =
      """out = [6, 8, 11]
        |out = [15, 23]
        |out = [15, 22]
        |out = []
        |""".stripMargin

//    Console.err.println(s"---- EXP ----\n\n$exp\n\n---- RES ----\n\n$res\n")

    assert (res === exp)
  }
}
