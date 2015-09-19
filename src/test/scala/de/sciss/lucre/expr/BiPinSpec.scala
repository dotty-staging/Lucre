package de.sciss.lucre.expr

import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

import scala.collection.immutable.{IndexedSeq => Vec}

object BiPinSpec {
  implicit class BiPinOps[S <: Sys[S], A](val `this`: BiPin.Entry[S, A]) extends AnyVal { me =>
    import me.{`this` => entry}

    def get: (LongObj[S], A) = (entry.key, entry.value)
  }
}
/*
  To run only this suite:

  test-only de.sciss.lucre.expr.BiPinSpec

  */
class BiPinSpec extends ConfluentEventSpec {
  import BiPinSpec.BiPinOps

  type LE = LongObj[S]
  type IE = IntObj [S]
  type E  = BiPin.Entry[S, IE]

  sealed trait Change1
  object Added {
    def apply(tup: (LE, IE))(implicit tx: S#Tx): Added = new Added(tup._1.value, tup)
  }
  case class Added  (time: Long, tup: (LE, IE)) extends Change1
  object Removed {
    def apply(tup: (LE, IE))(implicit tx: S#Tx): Removed = new Removed(tup._1.value, tup)
  }
  case class Removed(time: Long, tup: (LE, IE)) extends Change1
  case class Moved  (span: Change[Long], tup: (LE, IE)) extends Change1
  case class Update1(bip: BiPin[S, IE], changes: scala.List[Change1])

  def mapUpdate(in: Any /* BiPin.Update[S, IE] */): Update1 = in match {
    case u0: BiPin.Update[_, _] =>
      val u = u0.asInstanceOf[BiPin.Update[S, IE]]
      Update1(u.pin, u.changes.map {
        case BiPin.Added  (time, BiPin.Entry(k, v)) => Added  (time, (k, v))
        case BiPin.Removed(time, BiPin.Entry(k, v)) => Removed(time, (k, v))
        case BiPin.Moved  (time, BiPin.Entry(k, v)) => Moved  (time, (k, v))
      })
  }

  // type Entry[S <: Sys[S], A] = (LongObj[S], A)

  "BiPin" should "notify observers about all relevant collection events" in { system =>
    val obs = new Observation
    val bipH = system.step { implicit tx =>
      val bip = BiPin.Modifiable[S, IntObj]
      bip.changed.react(obs.register)
      val res = tx.newHandle(bip)(BiPin.Modifiable.serializer[S, IE])
      obs.assertEmpty()
      res
    }

    val tuples: Seq[stm.Source[S#Tx, (LE, IE)]] = system.step { implicit tx =>
      // implicitly[Serializer[S#Tx, S#Acc, BiPin.Entry[S, Expr[S, Int]]]]
      implicit val entrySer = BiPin.Entry.serializer[S, IE]

      val tup1 = (10000L: LE) -> (1: IE) // : E
      val tup2 = ( 5000L: LE) -> (2: IE) // : E
      val tup3 = (15000L: LE) -> (3: IE) // : E
      val tup4 = (20000L: LE) -> (4: IE) // : E
      val tup5 = (15000L: LE) -> (5: IE) // : E
      val tup6 = (15000L: LE) -> (6: IE) // : E
      Seq(tup1, tup2, tup3, tup4, tup5, tup6).map(tx.newHandle(_))
    }

    system.step { implicit tx =>
      val Seq(tup1, tup2, tup3, tup4, tup5, tup6) = tuples.map(_.apply())

      val bip = bipH()
      bip.add(tup1._1, tup1._2)
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from( 10000L ) -> (1: IntEx) ))
        Update1(bip, Added(tup1) :: Nil)
      )
      obs.clear()
      assert(bip.valueAt(tup1._1.value - 1) === None)
      assert(bip.valueAt(tup1._1.value    ) === Some(tup1._2))
      assert(bip.valueAt(tup1._1.value + 1) === Some(tup1._2))

      bip.add(tup2._1, tup2._2)
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 5000L, 10000L ) -> (2: IntEx) ))
        Update1(bip, Added(tup2) :: Nil)
      )
      obs.clear()

      bip.add(tup3._1, tup3._2)
      //         println( "at 10000 : " + bip.at( 10000L ))
      // note: the shrunken regions are _not_ fired!
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( /* Span( 10000L, 15000L ) -> (1: IntEx), */
        //                                            Span.from( 15000L ) -> (3: IntEx) ))
        Update1(bip, Added(tup3) :: Nil)
      )
      obs.clear()

      bip.add(tup4._1, tup4._2)
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from( 20000L ) -> (4: IntEx) ))
        Update1(bip, Added(tup4) :: Nil)
      )
      obs.clear()

      assert(bip.valueAt(tup3._1.value) === Some(tup3._2))
      bip.add(tup5._1, tup5._2) // should override the `3`
      assert(bip.valueAt(tup3._1.value) === Some(tup5._2))
      bip.add(tup6._1, tup6._2) // should override the `5`
      assert(bip.valueAt(tup3._1.value) === Some(tup6._2))

      assert(bip.intersect(tup3._1.value).map(_.get) === Vec[TUP](tup6, tup5, tup3)) // recent values first

      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 15000L, 20000L ) -> (5: IntEx) )),
        //            BiPin.Collection( bip, Vec( Span( 15000L, 20000L ) -> (6: IntEx) ))
        Update1(bip, Added(tup5) :: Nil),
        Update1(bip, Added(tup6) :: Nil)
      )
      obs.clear()
    }

    type TUP = (LE, IE)

    system.step { implicit tx =>
      val Seq(tup1: TUP, tup2: TUP, tup3: TUP, tup4: TUP, tup5: TUP, tup6: TUP) = tuples.map(_.apply())
      val bip = bipH()

      bip.remove(tup5._1, tup5._2) // should not be noticable
      assert(bip.valueAt  (tup3._1.value) === Some(tup6._2))
      assert(bip.intersect(tup3._1.value).map(_.get) === Vec[TUP](tup6, tup3))

      bip.remove(tup6._1, tup6._2) // should fall back to `3`
      assert(bip.valueAt  (tup3._1.value) === Some(tup3._2))
      assert(bip.intersect(tup3._1.value).map(_.get) === Vec[TUP](tup3))

      // tup5 removal not noticeable!
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 15000L, 20000L ) -> (3: IntEx) ))
        Update1(bip, Removed(tup6) :: Nil)
      )
      obs.clear()

      bip.remove(15000L: LE, 11: IE) // should be ignored
      bip.remove(15001L: LE, 3: IE) // should be ignored
      obs.assertEmpty()
      assert(bip.valueAt(tup3._1.value) === Some(tup3._2))

      bip.remove(tup3._1, tup3._2)
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 10000L, 20000L ) -> (1: IntEx) ))
        Update1(bip, Removed(tup3) :: Nil)
      )
      obs.clear()
      assert(bip.valueAt(tup3._1.value) === Some(tup1._2))

      bip.remove(tup4._1, tup4._2)
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from( 10000L ) -> (1: IntEx) ))
        Update1(bip, Removed(tup4) :: Nil)
      )
      obs.clear()

      bip.remove(tup2._1, tup2._2)
      bip.remove(tup1._1, tup1._2)
      //         obs.assertEmpty()
      obs.map(mapUpdate)
      obs.assertEquals(
        Update1(bip, Removed(tup2) :: Nil),
        Update1(bip, Removed(tup1) :: Nil)
      )
      obs.clear()

      assert(bip.intersect(0L).isEmpty && bip.intersect(20000L).isEmpty)
    }
  }

  "BiPin" should "notify observers about all relevant element events" in { system =>
    val obs = new Observation
    val bipH = system.step { implicit tx =>
      val bip = BiPin.Modifiable[S, IntObj]
      bip.changed.react(obs.register)
      val res = tx.newHandle(bip)(BiPin.Modifiable.serializer[S, IE])
      obs.assertEmpty()
      res
    }

    val (timeH, exprH) = system.step { implicit tx =>
      // partial currently broken
      //         val time = Longs.newVar[ S ]( 10000L )
      //         val expr = Ints.newVar[ S ]( 4 )
      val time  = LongObj.newVar /* newConfluentVar */ [S](10000L)
      val expr  = IntObj .newVar /* newConfluentVar */ [S](4)
      val th    = tx.newHandle(time -> (3: IE)) //  : E)
      val eh    = tx.newHandle((30000L: LE) -> expr) //  : E)
      (th, eh)
    }

    //      confluent.showLog = false

    val tup1H = system.step { implicit tx =>
      val tup1 = (    0L: LE) -> (1: IE) // : E
      val tup2 = (20000L: LE) -> (2: IE) // : E

      val bip  = bipH()
      bip.add(tup1._1, tup1._2)
      bip.add(tup2._1, tup2._2)
      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from(     0L ) -> (1: IntEx) )),
        //            BiPin.Collection( bip, Vec( Span.from( 20000L ) -> (2: IntEx) ))
        Update1(bip, Added(tup1) :: Nil),
        Update1(bip, Added(tup2) :: Nil)
      )
      obs.clear()

      val time = timeH()
      val expr = exprH()
      bip.add(time._1, time._2)
      bip.add(expr._1, expr._2)

      obs.map(mapUpdate)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 10000L, 20000L ) -> (3: IntEx) )),
        //            BiPin.Collection( bip, Vec( Span.from( 30000L ) -> expr ))
        Update1(bip, Added(time) :: Nil),
        Update1(bip, Added(expr) :: Nil)
      )
      obs.clear()

      tx.newHandle(tup1)
    }

    system.step { implicit tx =>
      val bip  = bipH()
      val time = timeH()
      val expr = exprH()

      val IntObj.Var(exprVar) = expr._2 // value

      exprVar() = 5
      obs.map(mapUpdate)
      obs.assertEquals()
//        //            BiPin.Element( bip, Vec( expr -> Change( 4, 5 )))
//        BiPin.Update[ S, Int ]( bip, Vec( BiPin.Element( expr, Change( 30000L -> 4, 30000L -> 5 ))))
//      )
      obs.clear()

      val LongObj.Var(timeVar) = time._1 // key
      timeVar() = 15000L
      //         println( "DEBUG " + bip.debugList() )
      //         println( "DEBUG " + bip.valueAt( 10000L ))
      val tup1 = tup1H()
      assert(bip.valueAt(10000L) === Some(tup1._2))
      assert(bip.valueAt(15000L) === Some(time._2))
      obs.map(mapUpdate)
      obs.assertEquals(
        Update1(bip, Moved(Change(10000L, 15000L), time) :: Nil)
      )
      obs.clear()

      timeVar() = -5000L
      assert( bip.valueAt(    -1L ) === Some( time._2 ))
      assert( bip.valueAt( 15001L ) === Some( tup1._2 ))
      obs.map(mapUpdate)
      obs.assertEquals(
        Update1(bip, Moved(Change(15000L, -5000L), time) :: Nil)
      )
      obs.clear()

      timeVar() = 25000L // the region -5000 ... 0 is 'swallowed' (NO: OBSOLETE)
      obs.map(mapUpdate)
      obs.assertEquals(
        Update1(bip, Moved(Change(-5000L, 25000L), time) :: Nil)
      )
      obs.clear()

      timeVar() = 35000L
      obs.map(mapUpdate)
      obs.assertEquals(
        Update1(bip, Moved(Change(25000L, 35000L), time) :: Nil)
      )
      obs.clear()

      exprVar() = 6
      obs.map(mapUpdate)
      obs.assertEquals(
        // BiPin.Update[S, IE](bip, BiPin.Moved(Change(30000L, 30000L), expr) :: Nil)
      )
      obs.clear()

      assert(bip.debugList().map { case (k, v) => (k, v.value) } ===
        scala.List(0L -> 1, 20000L -> 2, 30000L -> 6, 35000L -> 3))
    }
  }
}