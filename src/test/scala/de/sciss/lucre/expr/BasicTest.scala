package de.sciss.lucre.expr

import de.sciss.lucre.expr
import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.model.Change

object BasicTest {
  type S = Durable

  def main(args: Array[String]): Unit = {
//    val system  = InMemory()
//    type S      = InMemory
    val db      = BerkeleyDB.tmp()
    val system  = Durable(db)
    try {
      run(system)
    } finally {
      system.close()
    }
  }

  def run(system: S): Unit = {
    expr.init()

    import Ops._

    val res1 = system.step { implicit tx =>
      val a = Int.newConst[S](1234)
      val b = Int.newConst[S](5678)
      val c = b - a
      c.value
    }

    assert(res1 == 5678 - 1234)

    var observations = Vector.empty[Int]

    val (res2, aH) = system.step { implicit tx =>
      observations = Vector.empty

      val a = Int.newVar[S](1234)
      val b = Int.newVar[S](5678)
      val c = b - a


      c.changed.react { implicit tx => {
        case Change(_, now) => observations :+= now // println(s"c = $now")
      }}

      a() = 333
      b() = 666

      import Int.varSerializer

      (c.value, tx.newHandle(a))
    }

    assert(res2 == 666 - 333)
    assert(observations == Vector(5678 - 333, 666 - 333))

    system.step { implicit tx =>
      observations = Vector.empty
      val a = aH()
      a() = 444
    }

    assert(observations == Vector(666 - 444))
  }
}