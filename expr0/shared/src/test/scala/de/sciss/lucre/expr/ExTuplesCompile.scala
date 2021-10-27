package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{Act, Ex, Trig}

trait ExTuplesCompile {
//  def runWith(m: ExMap[String, Any]): Unit

  object Attr {
    import scala.language.implicitConversions

    implicit def fromExTuple[A](tup: A)(implicit view: A => Ex[(String, _)]): Attr = ???

    implicit def fromExAtom[A, B](tup: (A, B))(implicit key: A => Ex[String], value: B => Ex[_]): Attr = ???

    implicit def fromActAtom[A](tup: (A, Act))(implicit key: A => Ex[String]): Attr = ???

    implicit def fromTrigAtom[A](tup: (A, Trig))(implicit key: A => Ex[String]): Attr = ???
  }
  trait Attr

  def runWith1(m: (Ex[String], Ex[_])*): Unit

  def runWith2(m: Ex[(String, _)]*): Unit

  // 27-Oct-2021 : SoundProcesses issue #111
  def runWith3(m: Attr*): Unit

  def value: Ex[String]

  def key: Ex[String]

  def action: Act

  def trigger: Act

  trait Bang extends Act with Trig

  def bang: Bang

  //  runWith(
//    Map(("foo": Ex[String]) -> ("bar": Ex[String]))
//  )

//  runWith1("foo" -> "bar", "baz" -> 123)

  runWith2("foo" -> "bar", "baz" -> 123)

  runWith3("foo" -> "bar", "baz" -> 123)

  (1, 2): Ex[(Int, Int)]

//  runWith1("foo" -> title)

  runWith2("foo" -> value)

  runWith3(
    "key" -> value,
    key -> value,
    key -> "value",
    "callback" -> action,
    key -> action,
    "tr" -> trigger,
    key -> trigger,
//    "bang" -> bang, // ambiguous
  )

//  0: Ex[Int]  // ok
//  Some("foo"): Ex[Option[String]]  // ok
//  Some("foo": Ex[String]): Ex[Option[String]]   // forbidden

//  runWith(
//    Map(("foo": Ex[String]) -> title)
//  )
}
