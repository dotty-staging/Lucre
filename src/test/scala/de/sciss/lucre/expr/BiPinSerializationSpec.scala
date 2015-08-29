package de.sciss.lucre.expr

import de.sciss.lucre.bitemp.BiPin

/*
  To test only this suite:

  test-only de.sciss.lucre.expr.BiPinSerializationSpec

  */
class BiPinSerializationSpec extends ConfluentEventSpec {
  //   confluent.showLog = true

  "BiPin" should "serialize and deserialize" in { system =>
    val bipH = system.step { implicit tx =>
      val bip = BiPin.Modifiable[S, IntObj]
      tx.newHandle(bip)(BiPin.Modifiable.serializer[S, IntObj[S]])
    }
    //      val acc = system.step { implicit tx => tx.inputAccess }
    //      println( "--step; bipH = " + bipH + "; cursor pos = " + acc )

    //      implicit val bipSer = BiPin.Expr.Modifiable.serializer[ S, Int ]

    import Ops._
//    import Long.{serializer => longSer}
//    import Int .{serializer => intSer }

    val (keyH, valueH) = system.step { implicit tx =>
      val bip = bipH() // tx.refresh( acc, bip0 )
      val key   : LongObj[S] = 1234L
      val value : IntObj [S] = 5678
      bip.add(key -> value)
      (tx.newHandle(key), tx.newHandle(value))
    }

    system.step { implicit tx =>
      val key       = keyH()
      val value     = valueH()
      val bip       = bipH() // tx.refresh( acc, bip0 )
      assert(bip.intersect(key.value - 1).isEmpty, "BiPin should be empty before the first pin")
      val res       = bip.intersect(key.value)
      val expected  = IndexedSeq[BiPin.Entry[S, Expr[S, Int]]](key -> value)
      assert(res === expected) // , "Unexpected retrieval " + res )
    }
  }
}