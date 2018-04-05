package de.sciss.lucre.confluent

import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.{MutableImpl, MutableSerializer}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.serial.{DataInput, DataOutput}
import org.scalatest.{Matchers, Outcome, fixture}

/*

To run only this test:

test-only de.sciss.lucre.confluent.RefreshSpec

 */
class RefreshSpec extends fixture.FlatSpec with Matchers {
  type FixtureParam = stm.Cursor[Confluent]
  type S = Confluent

  // confluent.showLog = true

  object Entity {
    implicit object Ser extends MutableSerializer[S, Entity] {
      protected def readData(in: DataInput, id: S#Id)(implicit tx: S#Tx) = {
        val field = tx.readIntVar(id, in)
        new Entity(id, field)
      }
    }

    def apply(init: Int)(implicit tx: S#Tx): Entity = {
      val id = tx.newId()
      val field = tx.newIntVar(id, init)
      new Entity(id, field)
    }
  }

  class Entity(val id: S#Id, val field: S#Var[Int]) extends MutableImpl[S] {
    protected def disposeData()(implicit tx: S#Tx): Unit = field.dispose()

    protected def writeData(out: DataOutput): Unit = field.write(out)
  }

  def withFixture(test: OneArgTest): Outcome = {
    val system = Confluent(BerkeleyDB.tmp())
    try {
      val (_, cursor) = system.cursorRoot(_ => ())(implicit tx => _ => system.newCursor())
      test(cursor)
    }
    finally {
      system.close()
    }
  }

  "An entity" should "serialize and deserialize via tx.refresh" in { cursor =>
    val value = 1234
    val h = cursor.step { implicit tx =>
      val ent = Entity(value)
      tx.newHandle(ent)
    }
    val res = cursor.step { implicit tx =>
      val ent = h() // tx.refresh( csrStale, entStale )
      ent.field()
    }
    assert(res == value, res)
  }
}
