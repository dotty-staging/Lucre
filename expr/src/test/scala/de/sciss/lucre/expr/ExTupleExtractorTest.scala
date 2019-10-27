//package de.sciss.lucre.expr
//
//import scala.language.{higherKinds, implicitConversions}
//
//trait ExTupleExtractorTest {
//  trait Ex[+A] {
//    def value: A
//  }
//
//  def aSeq: Ex[Seq[String]]
//  def bSeq: Ex[Seq[String]]
//
//  trait CanMap[-From[_], -B, +To]
//
//  implicit def ExSeqOps   [A]   (in: Ex[Seq   [A]]) : ExSeqOps    [A]
//  implicit def ExTupleOps [A, B](in: Ex[(A, B)])    : ExTupleOps  [A, B]
//  implicit def ExStringOps      (in: Ex[String])    : ExStringOps
//
//  implicit def canMapExSeq[B]: CanMap[Seq, Ex[B], Ex[Seq[B]]]
//
//  abstract class ExSeqOps[A](x: Ex[Seq[A]]) {
//    def map[B, To](f: Ex[A] => B)(implicit m: CanMap[Seq, B, To]): To
//
//    def zip[B](that: Ex[Seq[B]]): Ex[Seq[(A, B)]]
//  }
//
//  abstract class ExTupleOps[A, B](x: Ex[(A, B)]) {
//    def _1 : Ex[A]
//    def _2 : Ex[B]
//  }
//
//  trait ExStringOps {
//    def nonEmpty: Ex[Boolean]
//
//    def ++(b: Ex[String]): Ex[String]
//  }
//
//  object Ex {
//    def unapply[A, B](tup: Ex[(A, B)]): Option[(Ex[A], Ex[B])] = ???
//  }
//
//  implicit def tupleForEx(t: Tuple2.type) = new {
//    def unapply[A, B](tup: Ex[(A, B)]): Option[(Ex[A], Ex[B])] = ???
//  }
//
//  val x = (1, 2)
//  val (y, z) = x
//
//  def extractorTest(): Unit = {
//    (aSeq zip bSeq).map { tup =>
//      tup._1 ++ tup._2  // ok
//    }
//
//    (aSeq zip bSeq).map { case (a, b) =>  // pattern type is incompatible
//      a ++ b
//    }
//  }
//}
