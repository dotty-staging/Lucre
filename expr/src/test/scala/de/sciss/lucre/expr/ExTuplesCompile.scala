package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.Ex

trait ExTuplesCompile {
//  def runWith(m: ExMap[String, Any]): Unit

  def runWith1(m: (Ex[String], Ex[_])*): Unit

  def runWith2(m: Ex[(String, _)]*): Unit

  def title: Ex[String]

//  runWith(
//    Map(("foo": Ex[String]) -> ("bar": Ex[String]))
//  )

//  runWith1("foo" -> "bar", "baz" -> 123)

  runWith2("foo" -> "bar", "baz" -> 123)

  (1, 2): Ex[(Int, Int)]

//  runWith1("foo" -> title)

  runWith2("foo" -> title)

//  0: Ex[Int]  // ok
//  Some("foo"): Ex[Option[String]]  // ok
//  Some("foo": Ex[String]): Ex[Option[String]]   // forbidden

//  runWith(
//    Map(("foo": Ex[String]) -> title)
//  )
}
