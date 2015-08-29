package de.sciss.lucre.stm

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import org.scalatest.{Outcome, Matchers, fixture}

/*
  to run only this test

  test-only de.sciss.lucre.stm.CopySpec

 */
class CopySpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  case class FixtureParam(s1: S, s2: S)

  protected def withFixture(test: OneArgTest): Outcome = {
    val s1 = Durable(BerkeleyDB.tmp())
    try {
      val s2 = Durable(BerkeleyDB.tmp())
      try {
        test(FixtureParam(s1, s2))
      } finally {
        s2.close()
      }
    } finally {
      s1.close()
    }
  }

  "Obj instances" should "be copyable across systems" in { param =>
    import param._
    val num1H = s1.step { implicit tx =>
      val num1 = IntObj.newVar(IntObj.newConst[S](1234))
      tx.newHandle(num1)
    }

    val num2H = Txn.copy[S, S, stm.Source[S#Tx, IntObj[S]]] { (tx1, tx2) =>
      val num1 = num1H()(tx1)
      val num2 = Obj.copy[S, S, IntObj](num1)(tx1, tx2)
      tx2.newHandle(num2)
    } (s1, s2)

    s1.step { implicit tx =>
      val num = num1H()
      assert(num.value === 1234)
    }

    s2.step { implicit tx =>
      val num = num2H()
      assert(num.value === 1234)
    }
  }
}