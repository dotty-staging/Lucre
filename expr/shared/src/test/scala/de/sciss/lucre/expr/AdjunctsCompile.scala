package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{CaseDef, Quote, Var}
import de.sciss.lucre.expr.ExImport._

import scala.annotation.unused

// ensure this compiles
trait AdjunctsCompile {
  def test(): Unit = {
    val intVar1       = Var[Int]()
    val booleanVar1   = Var[Boolean]()
    val doubleVar1    = Var[Double]()

    val intVar2       = Var[Int](33)
    val booleanVar2   = Var[Boolean](true)
    val doubleVar2    = Var[Double](1.2)

    val intSeqVar1    = Var[Seq[Int]]()
    val doubleSeqVar1 = Var[Seq[Double]]()

    val intSeqVar2    = Var[Seq[Int]](Seq(1, 2, 3))
    val doubleSeqVar2 = Var[Seq[Double]](Seq(1.0, 2.0, 3.0))

    use(intVar1)
    use(booleanVar1)
    use(doubleVar1)
    use(intVar2)
    use(booleanVar2)
    use(doubleVar2)
    use(intSeqVar1)
    use(doubleSeqVar1)
    use(intSeqVar2)
    use(doubleSeqVar2)

    val cd1: CaseDef[_]  = Quote(123)
    val cd2: CaseDef[_]  = intVar1
    val cd3: CaseDef[_]  = intSeqVar1

    use(cd1)
    use(cd2)
    use(cd3)

    val intAttr1        = "int"       .attr[Int]
    val booleanAttr1    = "boolean"   .attr[Boolean]
    val doubleAttr1     = "double"    .attr[Double]
    val intSeqAttr1     = "intSeq"    .attr[Seq[Int]]
    val doubleSeqAttr1  = "doubleSeq" .attr[Seq[Int]]

    val intAttr2        = "int"       .attr[Int](33)
    val booleanAttr2    = "boolean"   .attr[Boolean](true)
    val doubleAttr2     = "double"    .attr[Double](1.2)
    val intSeqAttr2     = "intSeq"    .attr[Seq[Int]](Seq(1, 2, 3))
    val doubleSeqAttr2  = "doubleSeq" .attr[Seq[Double]](Seq(1.0, 2.0, 3.0))

    use(intAttr1)
    use(booleanAttr1)
    use(doubleAttr1)
    use(intSeqAttr1)
    use(doubleSeqAttr1)
    use(intAttr2)
    use(booleanAttr2)
    use(doubleAttr2)
    use(intSeqAttr2)
    use(doubleSeqAttr2)
  }

  def use(@unused x: Any): Unit = ()
}
