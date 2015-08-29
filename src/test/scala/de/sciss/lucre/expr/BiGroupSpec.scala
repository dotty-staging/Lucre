package de.sciss.lucre.expr

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}

/*
 To run only this suite:

 test-only de.sciss.lucre.expr.BiGroupSpec

 */
class BiGroupSpec extends ConfluentEventSpec {

  // "OutOfMemoryError"
  //  _Span( 332000, 892000),
  //  _Span( 341000, 943000),
  //  _Span( 498000, 859000),
  //  _Span( 562000,1066000)

  "BiGroup" should "report correct event successions" in { system =>
    val spans0 = Vec(
      _Span( 3300000, 8900000),
      _Span( 3400000, 9400000),
      _Span( 4900000, 8500000),
      _Span( 5600000,10600000)
    )

    val spans1 = Vec(
      _Span.All,
      _Span(  150000, 7020000),
      _Span( 3300000, 8900000),
      _Span( 3370000, 9420000),
      _Span( 3400000, 9400000),
      _Span( 4900000, 8500000),
      _Span( 5600000,10600000),
      _Span( 7794219,10440219),
      _Span(10088174,14870035),
      _Span(10510112,11246044),
      _Span(10723389,10998126),
      _Span(10912234,11091796),
      _Span(11772841,15633738),
      _Span(12275921,14190692),
      _Span(12571937,19584395),
      _Span(12571937,19618564),
      _Span(13781584,15317197),
      _Span(14149413,14958486),
      _Span(14892439,17574299),
      _Span(15317197,15365248),
      _Span(15365248,15645544),
      _Span(15531230,17574299),
      _Span(15645544,15842019),
      _Span(15842019,15955205),
      _Span(15955205,16180259),
      _Span(16180259,18049306),
      _Span(17576436,19193246),
      _Span(17578570,19193246),
      _Span(18774670,20236860),
      _Span(18774670,24270017),
      _Span(18798496,24335649),
      _Span(18798496,24372145),
      _Span(24042282,24796636),
      _Span(24667937,25060737),
      _Span(28604671,32149986),
      _Span(28604671,32187329)
    )

    // val dummy = event.Dummy[S, Unit]
    // implicit val sl = SpanLike

    def withSpans(spanLikes: Vec[_SpanLike]): Unit = {
      val spans = spanLikes.collect {
        case sp @ _Span(_, _) => sp
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
        val g = BiGroup.Modifiable[S, IntObj]

        import Ops._
        spanLikes.foreach(g.add(_, 0))

        // println(g.debugPrint)

        (-1L +: manual).sliding(2, 1).foreach { case Vec(pred, succ) =>
          // println(s"Querying ${pred+1} - expecting $succ")
          val query = g.eventAfter(pred)
          assert(query === Some(succ))
        }
      }
    }

    withSpans(spans0)
    withSpans(spans1)
  }
}