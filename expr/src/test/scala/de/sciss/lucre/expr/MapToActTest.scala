package de.sciss.lucre.expr

import de.sciss.lucre.stm.{InMemory, Obj, UndoManager, Workspace}

/*
  expected output
  TODO: make this a real unit test

  [expand]
  [add]
  [update] Some(123)
  123
  [remove]
  [update] None
  [add]
  [update] Some(456)
  456

 */
object MapToActTest extends App {
  type S = InMemory

  val g = Graph {
    import ExImport._
    import graph._

    val opt: Ex[Option[String]] = "bar".attr[String]
    val actOpt  = opt.map { s =>
      PrintLn(s)
    }
    val act = actOpt // .orNop

    opt.changed ---> act
  }

  implicit val system: S = InMemory()
  implicit val undo: UndoManager[S] = UndoManager()

  import Workspace.Implicits._

  val (ctl, selfH, ctx) = system.step { implicit tx =>
    val self: Obj[S] = StringObj.newConst("foo")
    val _selfH = tx.newHandle(self)
    implicit val _ctx: Context[S] = Context[S](Some(_selfH))
    println("[expand]")
    val _ctl = g.expand
    _ctl.initControl()

    val view = CellView.attr[S, String, StringObj](self.attr, "bar")
    view.react { _ => upd =>
      println(s"[update] $upd")
    }

    (_ctl, _selfH, _ctx)
  }

  Thread.sleep(500)

  println("[add]")
  system.step { implicit tx =>
    val self = selfH()
    self.attr.put("bar", StringObj.newConst("123"))
  }

  Thread.sleep(500)
  println("[remove]")
  system.step { implicit tx =>
    val self = selfH()
    self.attr.remove("bar")
  }

  Thread.sleep(500)
  println("[add]")
  system.step { implicit tx =>
    val self = selfH()
    self.attr.put("bar", StringObj.newConst("456"))
  }

  Thread.sleep(500)
  system.step { implicit tx =>
    println("[dispose graph]")
    ctl.dispose()
    println("[dispose context]")
    ctx.dispose()
  }
}
