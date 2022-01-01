package de.sciss.lucre.expr

import de.sciss.serial.{DataInput, DataOutput}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/*

  sbt 'testOnly de.sciss.lucre.expr.ExSerializationSpec'

 */
class ExSerializationSpec extends AnyFlatSpec with Matchers {
  LucreExpr.init()

  "Issue No. 40" should "be fixed" in {
    val gIn = Graph {
      import graph._
      val sq: Ex[Seq[Obj]] = Seq(Folder(), Obj.empty)
      val sel = sq.select[Int]
      LoadBang() --> PrintLn(sel.mkString(", "))
    }

    val out = DataOutput()
    Graph.format.write(gIn, out)
    val in = DataInput(out.toByteArray)
    val gOut = Graph.format.read(in)

    assert(gIn === gOut)
  }
}