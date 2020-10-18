package de.sciss.lucre.expr

import de.sciss.lucre.{Confluent, Durable, confluent}
import de.sciss.lucre.store.BerkeleyDB
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.TxnLocal

trait ConfluentEventSpec extends FixtureAnyFlatSpec with Matchers {
  type S = Confluent
  type T = Confluent.Txn
  type D = Durable  .Txn
  type FixtureParam = confluent.Cursor[T, D]

//  implicit final protected val IntType  = lucre.expr.Int
//  implicit final protected val LongType = lucre.expr.Long

  LucreExpr.init()

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Confluent(BerkeleyDB.tmp())
    try {
      val (_, cursor) = system.cursorRoot(_ => ())(implicit tx => _ => system.newCursor())
      test(cursor)
    }
    finally {
      system.close()
    }
  }

  final class Observation /* [S <: stm.Sys[S]] */ {
    private val seqRef = TxnLocal(init = Vec.empty[Any])

    def map(fun: Any => Any)(implicit tx: T): Unit =
      seqRef.transform(_.map(fun))(tx.peer)

    def register(tx: T)(upd: Any): Unit =
      seqRef.transform(_ :+ upd)(tx.peer)

    def assertEquals(expected: Any*)(implicit tx: T): Unit = {
      val ob = seqRef.get(tx.peer)
      assert(ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString("[", ", ", "]")
        + "\n...but observed\n   " + ob.mkString("[", ", ", "]"))
    }

    def clear()(implicit tx: T): Unit =
      seqRef.set(Vector.empty)(tx.peer)

    def assertEmpty()(implicit tx: T): Unit = assertEquals()

    def print()(implicit tx: T): Unit =
      println(seqRef.get(tx.peer).mkString("[", ", ", "]"))
  }
}