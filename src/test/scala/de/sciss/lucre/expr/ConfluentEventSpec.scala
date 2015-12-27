package de.sciss.lucre.expr

import de.sciss.lucre.confluent
import de.sciss.lucre.confluent.Confluent
import de.sciss.lucre.stm.store.BerkeleyDB
import org.scalatest.{Matchers, Outcome, fixture}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.TxnLocal

trait ConfluentEventSpec extends fixture.FlatSpec with Matchers {
  type S = Confluent
  type D = S#D
  type FixtureParam = confluent.Cursor[S, D]

//  implicit final protected val IntType  = lucre.expr.Int
//  implicit final protected val LongType = lucre.expr.Long

  init()

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

    def map(fun: Any => Any)(implicit tx: S#Tx): Unit =
      seqRef.transform(_.map(fun))(tx.peer)

    def register(tx: S#Tx)(upd: Any): Unit =
      seqRef.transform(_ :+ upd)(tx.peer)

    def assertEquals(expected: Any*)(implicit tx: S#Tx): Unit = {
      val ob = seqRef.get(tx.peer)
      assert(ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString("[", ", ", "]")
        + "\n...but observed\n   " + ob.mkString("[", ", ", "]"))
    }

    def clear()(implicit tx: S#Tx): Unit =
      seqRef.set(Vector.empty)(tx.peer)

    def assertEmpty()(implicit tx: S#Tx): Unit = assertEquals()

    def print()(implicit tx: S#Tx): Unit =
      println(seqRef.get(tx.peer).mkString("[", ", ", "]"))
  }
}