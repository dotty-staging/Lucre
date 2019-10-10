package de.sciss.lucre.expr

// Note: IntelliJ highlights this all wrong.
trait StringLiteralOpsTest {
  import graph._
  import ExImport._

  def j : Ex[Int]
  def e : Ex[String]

  def run(): Unit = {
    val c = "foo"
    val d = "bar"
    val i = 33

    e.length
    c.length

    e.size
    c.size

    e.isEmpty
    c.isEmpty

    e.nonEmpty
    c.nonEmpty

    c ++ e
    c ++ d

    c.contains(e)
    c.contains(d)

    c.indexOf(e)
    c.indexOf(d)

    c.take(j)
    c.take(i)

    c.drop(j)
    c.drop(i)

    c.slice(j, j)
    c.slice(i, i)
    c.slice(i, j)
    c.slice(j, i)

    e.format(i)
    c.format(i)
  }
}
