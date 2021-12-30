/*
 *  IntHyperCubeN.scala
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

package de.sciss.lucre.geom

import de.sciss.serial.ConstFormat

import scala.collection.immutable.{IndexedSeq => Vec}

sealed trait IntHyperCubeNLike 
  extends HyperCube[IntPointNLike, IntHyperCubeN] with QueryShape[BigInt, IntPointNLike, IntHyperCubeN] {
  
  type P = IntPointNLike
  type H = IntHyperCubeN
  
  import Space.bigZero

  def dim: Int

  def center(idx: Int): Int

  /** The extent is the half side length of the cube */
  def extent: Int

  final def orthant(idx: Int): H = {
    val e = extent >> 1
    val d = Vector.tabulate(dim)(i => if ((idx & (1 << i)) == 0) -e else e)
    IntHyperCubeN(d, e)
  }

  final def containsP(point: P): Boolean = {
    val em1 = extent - 1
    var i = 0; while (i < dim) {
      val cc = center(i)
      val pc = point(i)
      if (cc - extent > pc || cc + em1 < pc) return false
      i += 1
    }
    true
  }

  final def containsH(cube: H): Boolean = {
    val be    = cube.extent
    val bem1  = be - 1
    val em1   = extent - 1

    var i = 0; while (i < dim) {
      val bcc = cube.center(i)
      val cc = center(i)
      if (bcc - be < cc - extent || bcc + bem1 > cc + em1) return false
      i += 1
    }
    true
  }

  final def area: BigInt = {
    val s = extent.toLong << 1
    BigInt(s).pow(dim)
  }

  // -- QueryShape --

  final def overlapArea(b: H): BigInt = {
    val be    = b.extent
    val bem1  = be - 1
    val em1   = extent - 1
    var prod  = Space.bigOne

    var i = 0; while (i < dim) {
      val bcc = b.center(i)
      val cc = center(i)
      val min = math.max(cc - extent, bcc - be).toLong
      val max = math.min(cc + em1, bcc + bem1).toLong
      val delta = max - min + 1
      if (delta <= 0L) return bigZero
      prod *= delta
      i += 1
    }

    prod
  }

  final def isAreaGreater(a: H, b: BigInt): Boolean = a.area > b

  final def isAreaNonEmpty(area: BigInt): Boolean = area > bigZero

  final def minDistance(point: P): Double =
    math.sqrt(minDistanceSq(point).toDouble) // or use this: http://www.merriampark.com/bigsqrt.htm ?

  final def maxDistance(point: P): Double =
    math.sqrt(maxDistanceSq(point).toDouble)

  /** The squared (euclidean) distance of the closest of the cube's corners
    * or sides to the point, if the point is outside the cube,
    * or zero, if the point is contained
    */
  final def minDistanceSq(point: P): BigInt = {
    val em1     = extent - 1
    var sumSqr  = bigZero

    var i = 0; while (i < dim) {
      val ac = point(i)
      val cc = center(i)
      val delta = if (ac < cc) {
        val min = cc - extent
        if (ac < min) min - ac else 0
      } else {
        val max = cc + em1
        if (ac > max) ac - max else 0
      }
      sumSqr += delta * delta
      i += 1
    }

    sumSqr
  }

  /** Calculates the maximum squared euclidean
    * distance to a point in the euclidean metric.
    * This is the distance (squared) to the corner which is the furthest from
    * the `point`, no matter if it lies within the hyper-cube or not.
    */
  final def maxDistanceSq(point: P): BigInt = {
    val em1     = extent - 1
    var sumSqr  = bigZero

    var i = 0
    while (i < dim) {
      val cc = center(i)
      val ac = point(i)
      val acl = ac.toLong
      val delta = if (ac < cc) {
        (cc + em1).toLong - acl
      } else {
        acl - (cc - extent).toLong
      }
      sumSqr += delta * delta
      i += 1
    }

    sumSqr
  }

  final def indexOfP(a: P) : Int = {
    val em1 = extent - 1
    var res = 0

    var i = 0; while (i < dim) {
      val ac = a(i)
      val cc = center(i)
      val pos = if (ac < cc) {
        if (ac >= cc - extent) 0 else return -1
      } else {
        if (ac <= cc + em1) 1 << i else return -1
      }
      res |= pos
      i += 1
    }

    res
  }

  final def indexOfH(b: H): Int = {
    val be    = b.extent
    val bem1  = be - 1
    val em1   = extent - 1
    var res   = 0

    var i = 0; while (i < dim) {
      val bcc  = b.center(i)
      val cc   = center(i)
      val bMin = bcc - be
      val bMax = bcc + bem1
      val pos = if (bcc < cc) {
        // not particular elegant to return in an assignment, but well, it's allowed :)
        if (bMin >= cc - extent && bMax < cc) 0 else return -1
      } else {
        // right?
        if (bMin >= cc && bMax <= cc + em1) 1 << i else return -1
      }
      res |= pos
      i += 1
    }

    res
  }

  final def greatestInterestingP(a: P, b: P): H = gi(a.components, 1, b)

  final def greatestInterestingH(a: H, b: P): H = {
    val ae = a.extent
    val ac = a.components.map(_ - ae)
    gi(ac, ae << 1, b)
  }

  private[this] def gi(a: Vec[Int], aSize: Int, b: P): H = {
    var mmc = Int.MaxValue
    var mi  = Int.MaxValue
    var i = 0; while (i < dim) {
      val cc  = center(i)
      val tlc = cc - extent
      val ac  = a(i)
      val akc = ac - tlc
      val bkc = b(i) - tlc
      val mc  = if (akc <= bkc) {
        IntSpace.binSplit(akc + aSize, bkc)
      } else {
        IntSpace.binSplit(bkc + 1, akc)
      }
      if (mc <= mmc) {
        mmc = mc
        mi  = i

      }
      i += 1
    }

    val components = Vector.tabulate(dim) { i =>
      val cc  = center(i)
      val tlc = cc - extent
      val ac  = a(i)
      val akc = ac - tlc
      val bkc = b(i) - tlc
      if (i != mi) {
        val c0 = math.min(akc, bkc)
        tlc + (c0 & (mmc << 1)) - mmc
      } else {
        // the coarsest dimension
        val c2 = math.max(akc, bkc)
        tlc + (c2 & mmc)
      }
    }

    IntHyperCubeN(components, -mmc)
  }
}

object IntHyperCubeN {
  implicit def format: ConstFormat[IntHyperCubeN] = IntSpace.NDim.hyperCubeFormat
}
final case class IntHyperCubeN(components: Vec[Int], extent: Int) extends IntHyperCubeNLike {
  def center(idx: Int): Int = components(idx)

  def dim: Int = components.size
}