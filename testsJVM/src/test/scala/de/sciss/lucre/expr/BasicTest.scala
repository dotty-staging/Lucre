package de.sciss.lucre.expr

import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Cursor, Durable, Expr, IntObj, Txn}
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

  def run[T <: Txn[T]](cursor: Cursor[T]): Unit = {
    LucreExpr.init()

//    import Ops._

    val res1 = cursor.step { implicit tx =>
      val a = IntObj.newConst[T](1234)
      val b = IntObj.newConst[T](5678)
      val c = b - a
      c.value
    }

    assert(res1 == 5678 - 1234)

    var observations = Vector.empty[Int]

    val (res2, aH) = cursor.step { implicit tx =>
      observations = Vector.empty

      val a = IntObj.newVar[T](1234)
      val b = IntObj.newVar[T](5678)
      val c = b - a


      c.changed.react { _ => {
        case Change(_, now) => observations :+= now // println(s"c = $now")
      }}

      implicitly[Int => IntObj[T]]
      implicitly[Int => Expr[T, Int]](implicitly[Int => IntObj[T]])

      a() = 333
      b() = 666

      // import Int.varSerializer

      (c.value, tx.newHandle(a))
    }

    assert(res2 == 666 - 333)
    assert(observations == Vector(5678 - 333, 666 - 333))

    cursor.step { implicit tx =>
      observations = Vector.empty
      val a = aH()
      a() = 444
    }

    assert(observations == Vector(666 - 444))
  }
}