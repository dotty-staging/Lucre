package de.sciss.lucre.expr

import de.sciss.lucre.{BiGroup, IntObj, SpanLikeObj, StringObj}
import de.sciss.span.{Span, SpanLike}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}

/*
 To run only this suite:

 testOnly de.sciss.lucre.expr.BiGroupSpec

 */
//object BiGroupSpec {
//  // helper
//  implicit class SeqMinus[A](val `this`: Seq[A]) extends AnyVal {
//    def - (elem: A): Seq[A] = -- (elem :: Nil)
//    def -- (that: Seq[A]): Seq[A] = `this` diff that
//  }
//}
class BiGroupSpec extends ConfluentEventSpec {

  // "OutOfMemoryError"
  //  Span( 332000, 892000),
  //  Span( 341000, 943000),
  //  Span( 498000, 859000),
  //  Span( 562000,1066000)

  "BiGroup" should "report correct event successions" in { system =>
    val spans0 = Vec(
      Span(3300000, 8900000),
      Span(3400000, 9400000),
      Span(4900000, 8500000),
      Span(5600000, 10600000)
    )

    val spans1 = Vec(
      Span.All,
      Span(  150000, 7020000),
      Span( 3300000, 8900000),
      Span( 3370000, 9420000),
      Span( 3400000, 9400000),
      Span( 4900000, 8500000),
      Span( 5600000,10600000),
      Span( 7794219,10440219),
      Span(10088174,14870035),
      Span(10510112,11246044),
      Span(10723389,10998126),
      Span(10912234,11091796),
      Span(11772841,15633738),
      Span(12275921,14190692),
      Span(12571937,19584395),
      Span(12571937,19618564),
      Span(13781584,15317197),
      Span(14149413,14958486),
      Span(14892439,17574299),
      Span(15317197,15365248),
      Span(15365248,15645544),
      Span(15531230,17574299),
      Span(15645544,15842019),
      Span(15842019,15955205),
      Span(15955205,16180259),
      Span(16180259,18049306),
      Span(17576436,19193246),
      Span(17578570,19193246),
      Span(18774670,20236860),
      Span(18774670,24270017),
      Span(18798496,24335649),
      Span(18798496,24372145),
      Span(24042282,24796636),
      Span(24667937,25060737),
      Span(28604671,32149986),
      Span(28604671,32187329)
    )

    // val dummy = event.Dummy[T, Unit]
    // implicit val sl = SpanLike

    def withSpans(spanLikes: Vec[SpanLike]): Unit = {
      val spans = spanLikes.collect {
        case sp @ Span(_, _) => sp
      }

      @tailrec def calcManual(res: Vec[Long] = Vec.empty): Vec[Long] = {
        val frame = res.lastOption.getOrElse(-1L) + 1
        val fut   = spans.filter(_.stop >= frame)
        if (fut.isEmpty) res else {
          val next  = fut.minBy(span => if (span.start >= frame) span.start else span.stop)
          val nextF = if (next.start >= frame) next.start else next.stop
          // println(s"Given $frame, next is $nextF")
          calcManual(res :+ nextF)
        }
      }

      val manual = calcManual()

      system.step { implicit tx =>
        val g = BiGroup.Modifiable[T, IntObj]

        // import Ops._
        spanLikes.foreach(g.add(_, 0))

        // println(g.debugPrint)

        (-1L +: manual).sliding(2, 1).foreach {
          case Vec(pred, succ) =>
            // println(s"Querying ${pred+1} - expecting $succ")
            val query = g.eventAfter(pred)
            assert(query === Some(succ))

          case _ => assert(false)
        }
      }
    }

    withSpans(spans0)
    withSpans(spans1)
  }

  "BiGroup" should "update cache correctly" in { system =>
    val (bipH, nH) = system.step { implicit tx =>
      val bip = BiGroup.Modifiable[T, IntObj]
      bip.add(Span(10L, 11L), 1)
      val n   = SpanLikeObj.newVar[T](Span(20L, 21L))
      bip.add(n, 2)
      bip.add(Span(30L, 31L), 3)
      (tx.newHandle(bip), tx.newHandle(n))
    }

    val list = system.step { implicit tx =>
      val n = nH()
      n() = Span(40L, 41L)
      val bip = bipH()

      def intersect(time: Long) =
        bip.intersect(time).map { case (s, vec) => (s, vec.map { e => (e.span.value, e.value.value) }) } .toList

      assert(intersect(10L) === scala.List((Span(10L, 11L), Vec((Span(10L, 11L), 1)))))
      assert(intersect(20L) === Nil)
      assert(intersect(30L) === scala.List((Span(30L, 31L), Vec((Span(30L, 31L), 3)))))
      assert(intersect(40L) === scala.List((Span(40L, 41L), Vec((Span(40L, 41L), 2)))))
      bip.debugList.map { case (m, p) => (m, p.value) }
    }
    assert(list === scala.List((Span(10L, 11L), 1), (Span(30L, 31L), 3), (Span(40L, 41L), 2)))
  }

  "A BiGroup" should "support correct range queries" in { system =>

    val putSpans = Seq[(SpanLike, String)](
      (Span.all           , "all"   ),
      (Span.from (10000)  , "from1" ),
      (Span.from (40000)  , "from2" ),
      (Span.from (42000)  , "from3" ),
      (Span.until( 9000)  , "until1"),
      (Span.until(10000)  , "until2"),
      (Span.until(41000)  , "until3"),
      (Span(-10000,  8000), "span1" ),
      (Span( 10000, 20000), "span2" ),
      (Span( 30000, 40000), "span3" ),
      (Span( 50000, 60000), "span4" ),
    )

    implicit val ser = BiGroup.Modifiable.format[T, StringObj[T]] // WTF -- why is this not inferred?
    val groupH = system.step { implicit tx =>
      val g = BiGroup.Modifiable[T, StringObj]
      putSpans.foreach { case (span, name) =>
        g.add(span, name)
      }
      tx.newHandle(g)
    }

    val points: Seq[Long] = putSpans.flatMap {
      case (Span(start, stop), _) => Seq(start - 1, start, start + 1, stop - 1, stop, stop + 1)
      case (Span.From(start), _)  => Seq(start - 1, start, start + 1)
      case (Span.Until(stop), _)  => Seq(stop - 1, stop , stop + 1)
      case _ => Nil
    } .distinct.sorted

    val testSpans = Seq(Span.all, Span.void) ++ points.map(Span.from) ++ points.map(Span.until) ++
      points.combinations(2).map { case Seq(start, stop) => Span(start, stop) } .toSeq

    system.step { implicit tx =>
      val g = groupH()
      for {
        start <- testSpans
        stop  <- testSpans
      } {
        val it  = g.rangeSearch(start = start, stop = stop)
        val res = it.flatMap(_._2.map(_.value.value)).toList
        val exp = putSpans.flatMap { case (span, name) =>
          val startOk = start match {
            case Span.All       => true
            case Span.Void      => false
            case Span.From (x)  => span.compareStart(x) >= 0
            case Span.Until(x)  => span.compareStart(x) <  0
            case Span(x, y)     => span.compareStart(x) >= 0 && span.compareStart(y) < 0
          }
          val stopOk = stop match {
            case Span.All       => true
            case Span.Void      => false
            case Span.From (x)  => span.compareStop(x) >= 0
            case Span.Until(x)  => span.compareStop(x) <  0
            case Span(x, y)     => span.compareStop(x) >= 0 && span.compareStop(y) < 0
          }
          if (startOk && stopOk) Some(name) else None
        }
        assert(res.sorted === exp.sorted, s", query-start = $start, query-stop = $stop")
      }
    }
  }
}