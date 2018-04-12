package de.sciss.lucre.stm

import org.scalatest.{FlatSpec, Matchers}

class PlainSpec extends FlatSpec with Matchers {
  "A Plain base system" should "behave as advertised" in {
    type S = Plain    // scalac bug -- it _is_ used
    val tx = Plain.instance

    val id = tx.newId()
    val vrS = tx.newVar(id, "Hello")
    assert(vrS() === "Hello")
    vrS() = "World"
    assert(vrS() === "World")

    val vrI = tx.newIntVar(id, 1)
    assert(vrI() === 1)
    vrI() = vrI() + 2
    assert(vrI() === 3)

    val id2 = tx.newId()
    assert(id !== id2)

    def alwaysFindsTx()(implicit tx: S#Tx): Unit = ()

    alwaysFindsTx()
  }
}
