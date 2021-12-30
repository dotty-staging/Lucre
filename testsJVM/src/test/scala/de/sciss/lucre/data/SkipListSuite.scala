/*
 *  SkipListSuite.scala
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

package de.sciss.lucre.data

import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.{Cursor, Durable, InMemory, Source, TestUtil, Txn}
import de.sciss.serial.TFormat
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

import scala.collection.immutable.IntMap
import scala.collection.mutable.{Set => MSet}
import scala.concurrent.stm.{InTxn, Ref, TxnExecutor}

/*

To run this test copy + paste the following into sbt:

test-only de.sciss.lucre.data.SkipListSuite

 */
class SkipListSuite extends AnyFeatureSpec with GivenWhenThen {
  val CONSISTENCY   = true
  val OBSERVATION   = true
  val REMOVAL       = true
  val TWO_GAP_SIZES = true
  val INMEMORY      = true
  val DATABASE      = true

  // large
  val NUM1 = if (TestUtil.isShort) 0x040000/4 else 0x040000
  val NUM2 = if (TestUtil.isShort) 0x020000/4 else 0x020000 // 0x100000

  require(NUM1 >= 1 && NUM2 >= 6)

  // small
  val NUM3 = 10

  val SEED = 0L

  val rnd = new util.Random(SEED)

  def withSys[S, T <: Txn[T]](sysName: String, sysCreator: () => (S, Cursor[T]), sysCleanUp: S => Unit): Unit = {
    if (TWO_GAP_SIZES) {
      withList[T](s"HA-1 ($sysName)", { oo =>
        val (sys, c) = sysCreator()
        val lH = c.step { implicit tx =>
          val l = HASkipList.Set.empty[T, Int](minGap = 1, keyObserver = oo)
          implicit val format: TFormat[T, HASkipList.Set[T, Int]] = HASkipList.Set.format(oo)
          tx.newHandle(l)
        }
        (c, lH, () => sysCleanUp(sys))
      })
    }
    withList[T](s"HA-2 ($sysName)", { oo =>
      val (sys, c) = sysCreator()
      val lH = c.step { implicit tx =>
        val l = HASkipList.Set.empty[T, Int](minGap = 2, keyObserver = oo)
        implicit val format: TFormat[T, HASkipList.Set[T, Int]] = HASkipList.Set.format(oo)
        tx.newHandle(l)
      }
      (c, lH, () => sysCleanUp(sys))
    })
  }

  if (INMEMORY) withSys("Mem", () => {
    val s = InMemory()
    (s, s)
  }, (_: InMemory) => ())
  
  if (DATABASE) {
    withSys[Durable, Durable.Txn]("BDB", () => {
      //      val dir = File.createTempFile("skiplist", "_database")
      //      dir.delete()
      //      dir.mkdir()
      //      println(dir.getAbsolutePath)
      val bdb = BerkeleyDB.tmp()
      val s = Durable(bdb)
      (s, s)
    }, s => {
      val sz = s.step(s.numUserRecords(_))
      assert(sz == 0, s"Final DB user size should be 0, but is $sz")
      s.close()
    })
  }

  def atomic[A](fun: InTxn => A): A = TxnExecutor.defaultAtomic(fun)

  def randFill[T <: Txn[T]](lH: Source[T, SkipList.Set[T, Int]], s: MSet[Int])(implicit cursor: Cursor[T]): Unit = {
    Given("a randomly filled structure")
    for (_ <- 0 until NUM1) {
      val x = rnd.nextInt(0x7FFFFFFF)
      s.add(x)
    }
    cursor.step { implicit tx =>
      val l = lH()
      s.foreach(l.add)
    }
  }

  def randFill2(): Set[Int] = {
    Given("a set of random numbers")
    var res = Set.empty[Int]
    for (_ <- 0 until NUM2) {
      res += rnd.nextInt() & ~1 // any int except MaxValue
    }
    res
  }

  def randFill3(): Set[Int] = {
    var res = Set.empty[Int]
    for (_ <- 0 until NUM3) {
      res += rnd.nextInt(100)
    }
    Given(s"a small set of numbers : ${res.mkString(", ")}")
    res
  }

  def verifyOrder[T <: Txn[T]](lH: Source[T, SkipList.Set[T, Int]])(implicit cursor: Cursor[T]): Unit = {
    When("the structure is mapped to its pairwise comparisons")
    //atomic { implicit tx => println( l.toList )}
    var res = Set.empty[Int]
    val seq = cursor.step { implicit tx => lH().toIndexedSeq }
    //      val iter = atomic( l.iterator( _ ))
    var prev = -2
    seq.foreach { next =>
      res += prev compare next
      prev = next
    }
    // res

    Then("the resulting set should only contain -1")
    assert(res == Set(-1), res.toString())
  }

  def verifyElems[T <: Txn[T]](lH: Source[T, SkipList.Set[T, Int]], s: MSet[Int])(implicit cursor: Cursor[T]): Unit = {
    When("the structure l is compared to an independently maintained set s")
    val ll      = cursor.step { implicit tx => lH().toIndexedSeq }
    val onlyInS = cursor.step { implicit tx => val l = lH(); s.filterNot(l.contains) }
    val onlyInL = ll.filterNot(s.contains)
    val szL     = cursor.step { implicit tx => lH().size }
    val szS     = s.size
    Then("all elements of s should be contained in l")
    assert(onlyInS.isEmpty, onlyInS.take(10).toString())
    Then("all elements of l should be contained in s")
    assert(onlyInL.isEmpty, onlyInL.take(10).toString())
    Then("both should report the same size")
    assert(szL == szS, s"skip list has size $szL / set has size $szS")

    When("the structure l is compared to the output from its iterator")
    Then("both should have the same size")
    assert(ll.size == szL, s"skip list has size $szL / iterator has size ${ll.size}")
  }

  def verifyContainsNot[T <: Txn[T]](lH: Source[T, SkipList.Set[T, Int]], s: MSet[Int])
                                    (implicit cursor: Cursor[T]): Unit = {
    When("the structure l is queried for keys not in the independently maintained set s")
    var testSet = Set.empty[Int]
    val inL = cursor.step { implicit tx =>
      val l = lH()
      while (testSet.size < 100) {
        val x = rnd.nextInt()
        if (!s.contains(x)) testSet += x
      }
      testSet.filter(l.contains)
    }
    Then("none of them should be contained in l")
    assert(inL.isEmpty, inL.take(10).toString())
  }

  def verifyAddRemoveAll[T <: Txn[T]](lH: Source[T, SkipList.Set[T, Int]], s: MSet[Int])
                                     (implicit cursor: Cursor[T]): Unit = {
    When("all elements of the independently maintained set are added again to l")
    val szBefore  = cursor.step { implicit tx => lH().size }
    val newInL    = cursor.step { implicit tx => val l = lH(); s.filter(l.add) }
    val szAfter   = cursor.step { implicit tx => lH().size }
    Then("none of the add operations should return 'true'")
    assert(newInL.isEmpty, newInL.take(10).toString())
    Then("the size of l should not change")
    assert(szBefore == szAfter, s"l had size $szBefore before, but now reports $szAfter")

    if (REMOVAL) {
      When("all elements of the independently maintained set are removed from l")
      val keptInL   = cursor.step { implicit tx => val l = lH(); s.filterNot(l.remove) }
      val szAfter2  = cursor.step { implicit tx => lH().size }
      Then("all of the remove operations should return 'true'")
      assert(keptInL.isEmpty, s"the following elements were not found in removal: ${keptInL.take(10).toString()}")
      Then("the size of l should be zero")
      assert(szAfter2 == 0, szAfter2.toString)
    }
  }

  private def withList[T <: Txn[T]](name: String,
                                    lf: SkipList.KeyObserver[T, Int] => (Cursor[T], Source[T, SkipList.Set[T, Int]],
                                      () => Unit)): Unit = {
    def scenarioWithTime(descr: String)(body: => Unit): Unit =
      Scenario(descr) {
        val t1 = System.currentTimeMillis()
        body
        val t2 = System.currentTimeMillis()
        println(s"For $name the tests took ${TestUtil.formatSeconds((t2 - t1) * 0.001)}")
      }

    if (CONSISTENCY) {
      Feature(s"The $name skip list structure should be consistent") {
        info("Several mass operations on the structure")
        info("are tried and expected behaviour verified")

        scenarioWithTime("Consistency is verified on a randomly filled structure") {
          val (_sys, lH, cleanUp) = lf(SkipList.NoKeyObserver)
          implicit val system: Cursor[T] = _sys
          try {
            val s = MSet.empty[Int]
            randFill(lH, s)
            verifyOrder(lH)
            verifyElems(lH, s)
            verifyContainsNot(lH, s)
            verifyAddRemoveAll(lH, s)
            system.step { implicit tx =>
              lH().dispose()
            }
          } finally {
            cleanUp()
          }
        }
      }
    }

    if (OBSERVATION) {
      Feature(s"The $name skip list structure should be observable") {
        info("Several operations are performed on the structure while an")
        info("observer monitors key promotions and demotions")

        scenarioWithTime("Observation is verified on a randomly filled structure") {
          val obs = new Obs[T]
          val (_sys, lH, cleanUp) = lf(obs)
          implicit val cursor: Cursor[T] = _sys
          try {
            val s = randFill2() // randFill3
            When("all the elements of the set are added")
            var uppedInsKey = false
            cursor.step { implicit tx =>
              val l = lH()
              s.foreach { i =>
                obs.oneUp.transform(_ - i)(tx.peer)
                l.add(i)
                uppedInsKey |= obs.oneUp()(tx.peer).contains(i)
              }
            }
            Then("none was ever promoted during their insertion")
            assert(!uppedInsKey)
            Then("there haven't been any demotions")
            assert(obs.allDn.single().isEmpty, obs.allDn.single().take(10).toString())
            Then("there were several promotions")
            assert( obs.allUp.single().nonEmpty )
            Then("the height of the list is equal to the maximally promoted key + 1")
            val maxProm = obs.allUp.single().maxBy(_._2)._2
            val (h, minGap, maxGap) = cursor.step { implicit tx => 
              val l = lH()
              (l.height, l.minGap, l.maxGap) 
            }
            assert(h == maxProm + 1, s"Height is reported as $h, while keys were maximally promoted $maxProm times")
            val sz      = s.size + 1 // account for the 'maxKey'
            val minH    = math.ceil(math.log(sz.toDouble) / math.log((maxGap + 1).toDouble)).toInt
            val maxH    = math.ceil(math.log(sz.toDouble) / math.log((minGap + 1).toDouble)).toInt
            Then("ceil(log(n+1)/log(maxGap+1)) <= height <= ceil(log(n+1)/log(minGap+1))")
            assert(minH <= h && h <= maxH, s"Not: $minH <= $h <= $maxH")

            if (REMOVAL) {
              When("all the elements are removed again")
              var uppedDelKey = false
              cursor.step { implicit tx =>
                val l = lH()
                s.foreach { i =>
                  obs.oneUp.transform(_ - i)(tx.peer)
                  l.remove(i)
                  uppedDelKey |= obs.oneUp()(tx.peer).contains(i)
                }
              }
              Then("none was ever promoted during their deletion")
              assert(!uppedDelKey, "elements were promoted during their deletion")
              val upsNotInS = obs.allUp.single().keys.filterNot(s.contains)
              Then("no key was ever promoted which was not in s")
              assert(upsNotInS.isEmpty, upsNotInS.take(10).toString())
              val dnsNotInS = obs.allDn.single().keys.filterNot(s.contains)
              Then("no key was ever demoted which was not in s")
              assert(dnsNotInS.isEmpty, dnsNotInS.take(10).toString())
              val unbal = atomic { implicit tx =>
                s.map(i => i -> (obs.allUp().getOrElse(i, 0) - obs.allDn().getOrElse(i, 0))).filterNot(_._2 == 0)
              }
              Then("the number of promotions and demotions for every key is equal")
              assert(unbal.isEmpty, unbal.take(10).toString())
            }
            cursor.step { implicit tx =>
              lH().dispose()
            }

          } finally {
            cleanUp()
          }
        }
      }
    }
  }

  final class Obs[T <: Txn[T]] extends SkipList.KeyObserver[T, Int] {
    val allUp: Ref[IntMap[Int]] = Ref(IntMap.empty[Int])
    val allDn: Ref[IntMap[Int]] = Ref(IntMap.empty[Int])
    val oneUp: Ref[IntMap[Int]] = Ref(IntMap.empty[Int])

    def keyUp(key: Int)(implicit tx: T) : Unit ={
      implicit val itx: InTxn = tx.peer
      allUp.transform(m => m + (key -> (m.getOrElse(key, 0) + 1)))
      oneUp.transform(m => m + (key -> (m.getOrElse(key, 0) + 1)))
    }

    def keyDown(key: Int)(implicit tx: T): Unit = {
      implicit val itx: InTxn = tx.peer
      allDn.transform(m => m + (key -> (m.getOrElse(key, 0) + 1)))
    }
  }
}