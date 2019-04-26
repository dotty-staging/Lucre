package de.sciss.lucre.expr

import de.sciss.lucre.expr.ExOps._
import de.sciss.lucre.expr.graph.{CaseDef, Quote, Var}

// ensure this compiles
trait AuxTest {
  def test(): Unit = {
    val intVar1     = Var[Int]()
    val booleanVar1 = Var[Boolean]()

    val intVar2     = Var[Int](33)
    val booleanVar2 = Var[Boolean](true)

    use(intVar1)
    use(booleanVar1)
    use(intVar2)
    use(booleanVar2)

    val cd1: CaseDef[_]  = Quote(123)
    val cd2: CaseDef[_]  = intVar1

    use(cd1)
    use(cd2)
  }

  def use(x: Any): Unit = ()
}
