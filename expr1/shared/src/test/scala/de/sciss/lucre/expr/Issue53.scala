package de.sciss.lucre.expr

import de.sciss.lucre.InMemory
import de.sciss.lucre
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/*

    testOnly de.sciss.lucre.expr.Issue53

 */
class Issue53 extends AnyFlatSpec with Matchers {
  LucreExpr.init()

  "Issue No. 53" should "be fixed" in {
    val gIn = Graph {
      import de.sciss.lucre.expr.ExImport._
      import graph._
      val in: Ex[Seq[Int]] = Seq(1, 2, 3)
      val f = "folder".attr(Folder())

      val actCreate = in.flatMap { x =>
        val name  = Const("int-%s").format(x)
        val obj   = x.asObj
        Act(
          obj.make,
          obj.attr[String]("name").set(name),
          f.append(obj)
        )
      }

      val lb = LoadBang()

      lb --> actCreate
    }

    type S = InMemory
    type T = InMemory.Txn
    implicit val system: S = InMemory()

    val res: Seq[(Int, String)] = system.step { implicit tx =>
      val f     = lucre.Folder[T]()
      val root  = lucre.Folder[T]()
      root.attr.put("folder", f)
      implicit val ws: lucre.Workspace[T] = lucre.Workspace.Implicits.dummy[T]
      implicit val undo: lucre.edit.UndoManager[T] = lucre.edit.UndoManager()
      implicit val ctx: lucre.expr.Context[T] = lucre.expr.Context(selfH = Some(tx.newHandle(root)))
      val ex = gIn.expand[T]
      ex.initControl()
      f.iterator.map { i =>
        val name = i.attr.$[lucre.StringObj]("name").fold("?")(_.value)
        (i.id.!.id, name)
      } .toList
    }

    val (isSq, nameSq) = res.unzip

//    println("RES: ")
//    println(isSq)
//    println(nameSq)

    assert (isSq === isSq.distinct)
    assert (nameSq === Seq("int-1", "int-2", "int-3"))
  }
}
