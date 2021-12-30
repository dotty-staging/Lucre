/*
 *  HashingSpec.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.confluent

import de.sciss.lucre.confluent.impl.PathImpl
import de.sciss.lucre.Confluent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.LongMap

/*

To run only this test:

test-only de.sciss.lucre.confluent.HashingSpec

 */
class HashingSpec extends AnyFlatSpec with Matchers {
  type S = Confluent
  type T = Confluent.Txn

  "The hashing mechanism" should "find correct prefix lengths" in {
    val p0 = PathImpl.empty[T] :+ 0x100000000L
    val p1 = p0 :+ 0x1234567800000001L
    val p2 = p1 :+ 0x2345678900000002L
    val p3 = p2 :+ 0x3456789000000003L
    val p4 = p3 :+ 0x4567890A00000004L

    val allPaths = Seq(p0, p1, p2, p3, p4)

    def test(parent: PathLike, expected: Seq[Int]): Unit = {
      var entries = LongMap.empty[Long]
      Hashing.foreachPrefix(parent, entries.contains) {
        case (_hash, _preSum) => entries += ((_hash, _preSum))
      }
      entries += ((parent.sum, 0L))
      val result = allPaths.map { p =>
        val l = Hashing.maxPrefixLength(p, entries.contains)
        l.max(0)
      }
      assert(result === expected)
    }

    test(p0, Seq(1, 1, 1, 1, 1))    // sz 1
    test(p1, Seq(0, 2, 2, 2, 2))    // sz 2
    test(p2, Seq(0, 2 /* 0 */, 3, 3, 3))    // sz 3
    test(p3, Seq(0, 0, 0, 4, 4))    // sz 4
    test(p4, Seq(0, 0, 0, 4 /* 0 */, 5))    // sz 5
  }
}
