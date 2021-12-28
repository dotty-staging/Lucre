package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{Ex, Folder}
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{BooleanObj, Durable, IntObj, Folder => LFolder}

object IntExObjTest {
  type S = Durable
  type T = Durable.Txn

  //  type S = InMemory
  //  type T = InMemory.Txn

  def main(args: Array[String]): Unit = {
    LucreExpr   .init()
//    LFolder       .init()
//    Ex            .init()
//    graph.Obj     .init()

    //    Log.event.level = Level.Debug

    //    implicit val system: S = InMemory()
    val store = BerkeleyDB.tmp()
    implicit val system: S = Durable(store)

    val (inH, outH) = system.step { implicit tx =>
      import ExImport._
      //      val ex: Ex[Int] = "in".attr(0) * 2
      val ex: Ex[Int] = "in".attr(Folder()).size
      //      val input     = IntObj.newVar[T](0)
      val input     = LFolder[T]()
      val transform = IntObj.newProgram[T](ex)
      println("--- put 'in'")
      transform.attr.put("in", input)
      val output    = IntObj.newVar[T](transform)
      (tx.newHandle(input), tx.newHandle(output))
    }

    system.step { implicit tx =>
      val out = outH()
      println("--- add react")
      out.changed.react { implicit tx => upd =>
        println(s"OBSERVED: $upd")
      }
    }

    system.step { implicit tx =>
      val in = inH()
      println("--- update 'in'")
      //      in() = 1000
      in.addLast(BooleanObj.newConst(false))
    }

    val v = system.step { implicit tx =>
      val out = outH()
      println("--- call 'value'")
      out.value
    }
    println(s"OUTPUT now $v")

    system.close()
  }
}