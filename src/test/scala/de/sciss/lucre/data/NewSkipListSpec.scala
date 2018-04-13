package de.sciss.lucre.data

import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import org.scalatest.{Matchers, Outcome, fixture}

class NewSkipListSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = Durable

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  "SkipList.Map" should "allow head/last, firstKey/lastKey operations" in { implicit sys =>
    sys.step { implicit tx =>
      val m = SkipList.Map.empty[S, Int, String]
      assertThrows[NoSuchElementException] { m.head     }
      assertThrows[NoSuchElementException] { m.last     }
      assertThrows[NoSuchElementException] { m.firstKey }
      assertThrows[NoSuchElementException] { m.lastKey  }

      m.put(12, "foo")
      assert(m.head === (12 -> "foo"))
      assert(m.last === (12 -> "foo"))
      assert(m.firstKey === 12)
      assert(m.lastKey  === 12)

      m.put(9, "bar")
      assert(m.head === (9 -> "bar"))
      assert(m.last === (12 -> "foo"))
      assert(m.firstKey === 9)
      assert(m.lastKey  === 12)

      (1 to 16 by 3).foreach(i => m.put(i, i.toString))
      assert(m.head === (1 -> "1"))
      assert(m.last === (16 -> "16"))
      assert(m.firstKey === 1)
      assert(m.lastKey  === 16)

      (1 to 16 by 3).foreach(i => m.remove(i))

      assert(m.head === (9 -> "bar"))
      assert(m.last === (12 -> "foo"))
      assert(m.firstKey === 9)
      assert(m.lastKey  === 12)
    }
  }
}
