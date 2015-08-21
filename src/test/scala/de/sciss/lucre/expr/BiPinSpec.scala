package de.sciss.lucre.expr

import de.sciss.lucre
import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.stm

import scala.collection.immutable.{IndexedSeq => Vec}

/*
  To run only this suite:

  test-only de.sciss.lucre.expr.BiPinSpec

  */
class BiPinSpec extends ConfluentEventSpec {
  type IntEx = Expr[S, Int]

  type LE = Expr[S, Long]
  type IE = Expr[S, Int ]
  type E  = BiPin.Entry[S, IE]

  "BiPin" should "notify observers about all relevant collection events" in { system =>
    val obs = new Observation
    val bipH = system.step { implicit tx =>
      val bip = BiPin.Modifiable[S, IE]
      bip.changed.react(obs.register)
      val res = tx.newHandle(bip)(BiPin.Modifiable.serializer[S, IE])
      obs.assertEmpty()
      res
    }

    import Ops._
    implicit val intSer = Int.serializer[S]

    val tuples: Seq[stm.Source[S#Tx, E]] = system.step { implicit tx =>
      // implicitly[Serializer[S#Tx, S#Acc, BiPin.Entry[S, Expr[S, Int]]]]
      implicit val entrySer = BiPin.Entry.serializer[S, IE]

      val tup1 = (10000L: LE) -> (1: IE) : E
      val tup2 = ( 5000L: LE) -> (2: IE) : E
      val tup3 = (15000L: LE) -> (3: IE) : E
      val tup4 = (20000L: LE) -> (4: IE) : E
      val tup5 = (15000L: LE) -> (5: IE) : E
      val tup6 = (15000L: LE) -> (6: IE) : E
      Seq(tup1, tup2, tup3, tup4, tup5, tup6).map(tx.newHandle(_))
    }

    system.step { implicit tx =>
      val Seq(tup1: E, tup2: E, tup3: E, tup4: E, tup5: E, tup6: E) = tuples.map(_.apply())

      val bip = bipH()
      bip.add(tup1)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from( 10000L ) -> (1: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Added(tup1.key.value, tup1) :: Nil)
      )
      obs.clear()
      assert(bip.valueAt(tup1.key.value - 1) === None)
      assert(bip.valueAt(tup1.key.value    ) === Some(tup1.value))
      assert(bip.valueAt(tup1.key.value + 1) === Some(tup1.value))

      bip.add(tup2)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 5000L, 10000L ) -> (2: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Added(tup2.key.value, tup2) :: Nil)
      )
      obs.clear()

      bip.add(tup3)
      //         println( "at 10000 : " + bip.at( 10000L ))
      // note: the shrunken regions are _not_ fired!
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( /* Span( 10000L, 15000L ) -> (1: IntEx), */
        //                                            Span.from( 15000L ) -> (3: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Added(tup3.key.value, tup3) :: Nil)
      )
      obs.clear()

      bip.add(tup4)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from( 20000L ) -> (4: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Added(tup4.key.value, tup4) :: Nil)
      )
      obs.clear()

      assert(bip.valueAt(tup3.key.value) === Some(tup3.value))
      bip.add(tup5) // should override the `3`
      assert(bip.valueAt(tup3.key.value) === Some(tup5.value))
      bip.add(tup6) // should override the `5`
      assert(bip.valueAt(tup3.key.value) === Some(tup6.value))

      assert(bip.intersect(tup3.key.value) === Vec[E](tup6, tup5, tup3)) // recent values first

      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 15000L, 20000L ) -> (5: IntEx) )),
        //            BiPin.Collection( bip, Vec( Span( 15000L, 20000L ) -> (6: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Added(tup5.key.value, tup5) :: Nil),
        BiPin.Update[S, IE](bip, BiPin.Added(tup6.key.value, tup6) :: Nil)
      )
      obs.clear()
    }

    system.step { implicit tx =>
      val Seq(tup1: E, tup2: E, tup3: E, tup4: E, tup5: E, tup6: E) = tuples.map(_.apply())
      val bip = bipH()

      bip.remove(tup5) // should not be noticable
      assert(bip.valueAt  (tup3.key.value) === Some(tup6.value))
      assert(bip.intersect(tup3.key.value) === Vec[E](tup6, tup3))

      bip.remove(tup6) // should fall back to `3`
      assert(bip.valueAt  (tup3.key.value) === Some(tup3.value))
      assert(bip.intersect(tup3.key.value) === Vec[E](tup3))

      // tup5 removal not noticable!
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 15000L, 20000L ) -> (3: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Removed(tup6.key.value, tup6) :: Nil)
      )
      obs.clear()

      bip.remove((15000L: LE) -> (11: IE)) // should be ignored
      bip.remove((15001L: LE) -> ( 3: IE)) // should be ignored
      obs.assertEmpty()
      assert(bip.valueAt(tup3.key.value) === Some(tup3.value))

      bip.remove(tup3)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 10000L, 20000L ) -> (1: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Removed(tup3.key.value, tup3) :: Nil)
      )
      obs.clear()
      assert(bip.valueAt(tup3.key.value) === Some(tup1.value))

      bip.remove(tup4)
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from( 10000L ) -> (1: IntEx) ))
        BiPin.Update[S, IE](bip, BiPin.Removed(tup4.key.value, tup4) :: Nil)
      )
      obs.clear()

      bip.remove(tup2)
      bip.remove(tup1)
      //         obs.assertEmpty()
      obs.assertEquals(
        BiPin.Update[S, IE](bip, BiPin.Removed(tup2.key.value, tup2) :: Nil),
        BiPin.Update[S, IE](bip, BiPin.Removed(tup1.key.value, tup1) :: Nil)
      )
      obs.clear()

      assert(bip.intersect(0L).isEmpty && bip.intersect(20000L).isEmpty)
    }
  }

  "BiPin" should "notify observers about all relevant element events" in { system =>
    val obs = new Observation
    val bipH = system.step { implicit tx =>
      val bip = BiPin.Modifiable[S, IE]
      bip.changed.react(obs.register)
      val res = tx.newHandle(bip)(BiPin.Modifiable.serializer[S, IE])
      obs.assertEmpty()
      res
    }

    implicit val intVarSer  = lucre.expr.Int .varSerializer[ S ]
    implicit val longVarSer = lucre.expr.Long.varSerializer[ S ]

    //      confluent.showLog = true
    import Ops._

    val (timeH, exprH) = system.step { implicit tx =>
      // partial currently broken
      //         val time = Longs.newVar[ S ]( 10000L )
      //         val expr = Ints.newVar[ S ]( 4 )
      val time = lucre.expr.Long.newVar /* newConfluentVar */[ S ]( 10000L )
      val expr = lucre.expr.Int .newVar /* newConfluentVar */[ S ]( 4 )
      val th   = tx.newHandle( time -> (3: IE) : E)
      val eh   = tx.newHandle( (30000L: LE) -> expr : E)
      (th, eh)
    }

    //      confluent.showLog = false

    val tup1H = system.step { implicit tx =>
      val tup1 = (    0L: LE) -> (1: IE) : E
      val tup2 = (20000L: LE) -> (2: IE) : E

      val bip  = bipH()
      bip.add( tup1 )
      bip.add( tup2 )
      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span.from(     0L ) -> (1: IntEx) )),
        //            BiPin.Collection( bip, Vec( Span.from( 20000L ) -> (2: IntEx) ))
        BiPin.Update[ S, IE ]( bip, BiPin.Added( tup1.key.value, tup1) :: Nil),
        BiPin.Update[ S, IE ]( bip, BiPin.Added( tup2.key.value, tup2) :: Nil)
      )
      obs.clear()

      val time = timeH()
      val expr = exprH()
      bip.add( time )
      bip.add( expr )

      obs.assertEquals(
        //            BiPin.Collection( bip, Vec( Span( 10000L, 20000L ) -> (3: IntEx) )),
        //            BiPin.Collection( bip, Vec( Span.from( 30000L ) -> expr ))
        BiPin.Update[ S, IE ]( bip, BiPin.Added( 10000L, time) :: Nil),
        BiPin.Update[ S, IE ]( bip, BiPin.Added( 30000L, expr) :: Nil)
      )
      obs.clear()

      tx.newHandle(tup1)
    }

    system.step { implicit tx =>
      val bip  = bipH()
      val time = timeH()
      val expr = exprH()

      val Expr.Var( exprVar ) = expr.value

      exprVar() = 5
      obs.assertEquals()
//        //            BiPin.Element( bip, Vec( expr -> Change( 4, 5 )))
//        BiPin.Update[ S, Int ]( bip, Vec( BiPin.Element( expr, Change( 30000L -> 4, 30000L -> 5 ))))
//      )
      obs.clear()

      val Expr.Var( timeVar ) = time.key
      timeVar() = 15000L
      //         println( "DEBUG " + bip.debugList() )
      //         println( "DEBUG " + bip.valueAt( 10000L ))
      val tup1 = tup1H()
      assert( bip.valueAt( 10000L ) === Some( tup1.value ))
      assert( bip.valueAt( 15000L ) === Some( time.value))
      obs.assertEquals()
//        //            BiPin.Collection( bip, Vec( Span(     0L, 15000L ) -> (1: IntEx),
//        //                                            Span( 15000L, 20000L ) -> (3: IntEx) ))
//        BiPin.Update[ S, Int ]( bip, Vec( BiPin.Element( time, Change( 10000L -> 3, 15000L -> 3 ))))
//      )
      obs.clear()

      timeVar() = -5000L
      assert( bip.valueAt(    -1L ) === Some( time.value ))
      assert( bip.valueAt( 15001L ) === Some( tup1.value ))
      obs.assertEquals()
//        //            BiPin.Collection( bip, Vec( Span(     0L, 20000L ) -> (1: IntEx),
//        //                                            Span( -5000L,     0L ) -> (3: IntEx) ))
//        BiPin.Update[ S, Int ]( bip, Vec( BiPin.Element( time, Change( 15000L -> 3, -5000L -> 3 ))))
//      )
      obs.clear()

      timeVar() = 25000L // the region -5000 ... 0 is 'swallowed' (NO: OBSOLETE)
      obs.assertEquals()
//        //            BiPin.Collection( bip, Vec( Span( 25000L, 30000L ) -> (3: IntEx) ))
//        BiPin.Update[ S, Int ]( bip, Vec( BiPin.Element( time, Change( -5000L -> 3, 25000L -> 3 ))))
//      )
      obs.clear()

      timeVar() = 35000L
      obs.assertEquals()
//        //            BiPin.Collection( bip, Vec( Span( 20000L, 30000L ) -> (2: IntEx),
//        //                                            Span.from( 35000L )    -> (3: IntEx) ))
//        BiPin.Update[ S, Int ]( bip, Vec( BiPin.Element( time, Change( 25000L -> 3, 35000L -> 3 ))))
//      )
      obs.clear()

      exprVar() = 6
      obs.assertEquals()
//        //            BiPin.Element( bip, Vec( expr -> Change( 5, 6 )))
//        BiPin.Update[ S, Int ]( bip, Vec( BiPin.Element( expr, Change( 30000L -> 5, 30000L -> 6 ))))
//      )
      obs.clear()

      assert( bip.debugList() === scala.List( 0L -> 1, 20000L -> 2, 30000L -> 6, 35000L -> 3 ))
    }
  }
}