package de.sciss.lucre.expr

import de.sciss.lucre.stm.{InMemory, Obj, Workspace}

object MapToActTest extends App {
  type S = InMemory

  val g = Graph {
    import ExOps._
    import graph._

    val opt: Ex[Option[String]] = "bar".attr[String]
    val actOpt  = opt.map { s =>
      PrintLn(s)
    }
    val act = actOpt.orNop

    opt.changed ---> act
  }

  implicit val system: S = InMemory()

  import Workspace.Implicits._

  val (ctl, selfH) = system.step { implicit tx =>
    val self: Obj[S] = StringObj.newConst("foo")
    val _selfH = tx.newHandle(self)
    implicit val ctx: Context[S] = Context[S](Some(_selfH))
    println("[expand]")
    val _ctl = g.expand
    _ctl.initControl()

    val view = CellView.attr[S, String, StringObj](self.attr, "bar")
    view.react { implicit tx => upd =>
      println(s"[update] $upd")
    }

    (_ctl, _selfH)
  }

  Thread.sleep(500)

  println("[add]")
  system.step { implicit tx =>
    val self = selfH()
    self.attr.put("bar", StringObj.newConst("123"))
  }

//  Thread.sleep(500)
//  println("[remove]")
//  system.step { implicit tx =>
//    val self = selfH()
//    self.attr.remove("bar")
//  }

  Thread.sleep(500)
  println("[add]")
  system.step { implicit tx =>
    val self = selfH()
    self.attr.put("bar", StringObj.newConst("456"))
  }

  Thread.sleep(500)
  system.step { implicit tx =>
    ctl.dispose()
  }
}
