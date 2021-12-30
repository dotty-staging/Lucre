/*
 *  DistanceMeasure3D.scala
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

package de.sciss.lucre
package geom

object IntDistanceMeasure3D {
  import IntSpace.ThreeDim
  import ThreeDim.{PointLike, HyperCube => Cube3D}
  import Space.{bigZero => Zero}
  import DistanceMeasure.Ops

  type Sqr  = BigInt
  type ML   = DistanceMeasure[Long, IntPoint3DLike, IntCube] with Ops[Long, IntPoint3DLike, IntCube]
  type MS   = DistanceMeasure[Sqr , IntPoint3DLike, IntCube] with Ops[Sqr , IntPoint3DLike, IntCube]

  private val MaxDistance: Sqr = {
    val n = BigInt(Long.MaxValue)
    n * n
  }

  /**
   * A measure that uses the euclidean squared distance
   * which is faster than the euclidean distance as the square root
   * does not need to be taken.
   */
  val euclideanSq: MS = EuclideanSq

  /**
   * A chebyshev distance measure, based on the maximum of the absolute
   * distances across the first two dimensions. The 3rd dimension is ignored!
   */
  val chebyshevXY: ML = ChebyshevXY

    /**
     * An 'inverted' chebyshev distance measure, based on the *minimum* of the absolute
     * distances across the first two dimensions. The 3rd dimension is ignored!
     * This is, strictly speaking, only a semi metric.
     */
    val invertedChebyshevXY: ML = InvertedChebyshevXY

  private object EuclideanSq extends SqrImpl {
    override def toString = "IntDistanceMeasure3D.euclideanSq"

    def distance   (a: PointLike, b: PointLike): Sqr = b.distanceSq   (a)
    def minDistance(a: PointLike, b: Cube3D   ): Sqr = b.minDistanceSq(a)
    def maxDistance(a: PointLike, b: Cube3D   ): Sqr = b.maxDistanceSq(a)
  }

  private sealed trait ClipLike[M] extends DistanceMeasure[M, IntPoint3DLike, IntCube] {
    protected def underlying: DistanceMeasure[M, IntPoint3DLike, IntCube]

    protected def clipping: Cube3D

    def distance   (a: PointLike, b: PointLike): M = if (clipping.containsP(b)) underlying.distance   (a, b) else maxValue
    def minDistance(a: PointLike, b: Cube3D   ): M = if (clipping.containsH(b)) underlying.minDistance(a, b) else maxValue
    def maxDistance(a: PointLike, b: Cube3D   ): M = if (clipping.containsH(b)) underlying.maxDistance(a, b) else maxValue
  }

  private final class SqrClip(protected val underlying: SqrImpl, protected val clipping: Cube3D)
    extends ClipLike[BigInt] with SqrImpl

  private final class LongClip(protected val underlying: LongImpl, protected val clipping: Cube3D)
    extends ClipLike[Long] with LongImpl

  private sealed trait ApproximateLike[M] extends Impl[M] {
    protected def underlying: DistanceMeasure[M, IntPoint3DLike, IntCube]
    protected def thresh: M

    def minDistance(a: PointLike, b: Cube3D): M = underlying.minDistance(a, b)
    def maxDistance(a: PointLike, b: Cube3D): M = underlying.maxDistance(a, b)

    def distance(a: PointLike, b: PointLike): M = {
      val res = underlying.distance(a, b) // b.distanceSq( a )
      if (isMeasureGreater(res, thresh)) res else zeroValue
    }
  }

  private final class SqrApproximate(protected val underlying: SqrImpl, protected val thresh: Sqr)
    extends ApproximateLike[Sqr] with SqrImpl

  private final class LongApproximate(protected val underlying: LongImpl, protected val thresh: Long)
    extends ApproximateLike[Long] with LongImpl

  private sealed trait OrthantLike[M]
    extends DistanceMeasure[M, IntPoint3DLike, IntCube] {

    protected def underlying: DistanceMeasure[M, IntPoint3DLike, IntCube]

    protected def idx: Int

    private[this] val right  = (idx & 1) != 0
    private[this] val bottom = (idx & 2) != 0
    private[this] val back   = (idx & 4) != 0

    override def toString = s"$underlying.quadrant($idx)"

    def distance(a: PointLike, b: PointLike): M = {
      if ((if (right ) b.x >= a.x else b.x <= a.x) &&
          (if (bottom) b.y >= a.y else b.y <= a.y) &&
          (if (back  ) b.z >= a.z else b.z <= a.z)) {

        underlying.distance(a, b)
      } else maxValue
    }

    def minDistance(p: PointLike, q: Cube3D): M = {
      val qe    = q.extent
      val qem1  = qe - 1

      if ((if (right ) (q.cx + qem1) >= p.x else (q.cx - qe) <= p.x) &&
          (if (bottom) (q.cy + qem1) >= p.y else (q.cy - qe) <= p.y) &&
          (if (back  ) (q.cz + qem1) >= p.z else (q.cz - qe) <= p.z)) {

        underlying.minDistance(p, q)
      } else maxValue
    }

    def maxDistance( p: PointLike, q: Cube3D ) : M = {
      val qe    = q.extent
      val qem1  = qe - 1

      if ((if (right ) (q.cx - qe) >= p.x else (q.cx + qem1) <= p.x) &&
          (if (bottom) (q.cy - qe) >= p.y else (q.cy + qem1) <= p.y) &&
          (if (back  ) (q.cz - qe) >= p.z else (q.cz + qem1) <= p.z)) {

        underlying.maxDistance(p, q)
      } else maxValue
    }
  }

  private final class SqrOrthant(protected val underlying: DistanceMeasure[BigInt, IntPoint3DLike, IntCube],
                                 protected val idx: Int)
    extends OrthantLike[BigInt] with SqrImpl

  private final class LongOrthant(protected val underlying: DistanceMeasure[Long, IntPoint3DLike, IntCube], 
                                  protected val idx: Int)
    extends OrthantLike[Long] with LongImpl

  private final class SqrExceptOrthant(protected val underlying: DistanceMeasure[BigInt, IntPoint3DLike, IntCube],
                                       protected val idx: Int)
    extends OrthantLike[BigInt] with SqrImpl

  private final class LongExceptOrthant(protected val underlying: DistanceMeasure[Long, IntPoint3DLike, IntCube],
                                        protected val idx: Int)
    extends OrthantLike[Long] with LongImpl

  private object ChebyshevXY extends ChebyshevXYLike {
    override def toString = "IntDistanceMeasure3D.chebyshevXY"

    protected def apply   (dx: Long, dy: Long): Long = math.max(dx, dy) // math.min(dx, dy) // math.max(dx, dy)
    protected def applyMin(dx: Long, dy: Long): Long = math.max(dx, dy) // math.min(dx, dy)
    protected def applyMax(dx: Long, dy: Long): Long = math.max(dx, dy)
  }

  private object InvertedChebyshevXY extends ChebyshevXYLike {
    override def toString = "IntDistanceMeasure3D.invertedChebyshevXY"

    protected def apply   (dx: Long, dy: Long): Long = math.min(dx, dy)
    protected def applyMin(dx: Long, dy: Long): Long = math.min(dx, dy)
    protected def applyMax(dx: Long, dy: Long): Long = math.min(dx, dy) // math.min(dx, dy)
  }

  private sealed trait ChebyshevXYLike extends LongImpl {
    protected def apply   (dx: Long, dy: Long): Long
    protected def applyMin(dx: Long, dy: Long): Long
    protected def applyMax(dx: Long, dy: Long): Long

    def distance(a: PointLike, b: PointLike): Long = {
      val dx = math.abs(a.x.toLong - b.x.toLong)
      val dy = math.abs(a.y.toLong - b.y.toLong)
      apply(dx, dy)
    }

    def minDistance(a: PointLike, q: Cube3D): Long = {
      val px    = a.x
      val py    = a.y
      val qe    = q.extent
      val qem1  = qe - 1
      val qcx   = q.cx
      val qcy   = q.cy
      val l     = qcx - qe // q.left
      val t     = qcy - qe // q.top
      var dx    = 0L
      var dy    = 0L
      if (px < l) {
        dx = l.toLong - px.toLong
        if (py < t) {
          dy = t.toLong - py.toLong
        } else {
          val b = qcy + qem1 // q.bottom
          if (py > b) {
            dy = py.toLong - b.toLong
          }
        }
      } else {
        val r = qcx + qem1 // q.right
        if (px > r) {
          dx = px.toLong - r.toLong
          if (py < t) {
            dy = t.toLong - py.toLong
          } else {
            val b = qcy + qem1 // q.bottom
            if (py > b) {
              dy = py.toLong - b.toLong
            }
          }
        } else if (py < t) {
          dy = t.toLong - py.toLong
          if (px < l) {
            dx = l.toLong - px.toLong
          } else {
            if (px > r) {
              dx = px.toLong - r.toLong
            }
          }
        } else {
          val b = qcy + qem1 // q.bottom
          if (py > b) {
            dy = py.toLong - b.toLong
            if (px < l) {
              dx = l.toLong - px.toLong
            } else {
              if (px > r) {
                dx = px.toLong - r.toLong
              }
            }
          }
        }
      }
      applyMin(dx, dy)
    }

    def maxDistance( a: PointLike, q: Cube3D ) : Long = {
      val px    = a.x
      val py    = a.y
      val qcx   = q.cx
      val qcy   = q.cy
      val qe    = q.extent
      val qem1  = qe - 1
      if (px < qcx) {
        val dx = (qcx + qem1).toLong - px.toLong
        val dy = if (py < qcy) {
          // bottom right is furthest
          (qcy + qem1).toLong - py.toLong
        } else {
          // top right is furthest
          py.toLong - (qcy - qe).toLong
        }
        apply(dx, dy)
      } else {
        val dx = px.toLong - (qcx - qe).toLong
        val dy = if (py < qcy) {
          // bottom left is furthest
          (qcy + qem1).toLong - py.toLong
        } else {
          // top left is furthest
          py.toLong - (qcy - qe).toLong
        }
        applyMax(dx, dy)
      }
    }
  }

  sealed trait Impl[M] extends Ops[M, IntPoint3DLike, IntCube] {
    protected def zeroValue: M
  }

  trait SqrImpl extends Impl[Sqr] {

    final def newArray(size: Int) = new Array[BigInt](size)

    final def maxValue : Sqr = MaxDistance
    final def zeroValue: Sqr = Zero

    final def isMeasureZero   (m: BigInt): Boolean = m == Zero
    final def isMeasureGreater(a: BigInt, b: BigInt): Boolean = a > b

    final def compareMeasure(a: BigInt, b: BigInt): Int = if (a > b) 1 else if (a < b) -1 else 0

    final def clip(quad: Cube3D): MS = new SqrClip(this, quad)

    final def approximate(thresh: BigInt): MS = new SqrApproximate(this, thresh)

    final def orthant(idx: Int): MS = {
      require(idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")")
      new SqrOrthant(this, idx)
    }

    final def exceptOrthant(idx: Int): MS = {
      require(idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")")
      new SqrExceptOrthant(this, idx)
    }
  }

  trait LongImpl extends Impl[Long] {
    //      final def manifest : Manifest[ Long ] = Manifest.Long
    final def newArray(size: Int) = new Array[Long](size)

    final def maxValue : Long = Long.MaxValue
    final def zeroValue: Long = 0L

    final def isMeasureZero   (m: Long): Boolean = m == 0L
    final def isMeasureGreater(a: Long, b: Long): Boolean = a > b

    final def compareMeasure(a: Long, b: Long): Int = if (a > b) 1 else if (a < b) -1 else 0

    final def clip       (quad: Cube3D): ML = new LongClip       (this, quad  )
    final def approximate(thresh: Long)   : ML = new LongApproximate(this, thresh)

    final def orthant(idx: Int): ML = {
      require(idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")")
      new LongOrthant(this, idx)
    }

    final def exceptOrthant(idx: Int): ML = {
      require(idx >= 0 && idx < 8, "Orthant index out of range (" + idx + ")")
      new LongExceptOrthant(this, idx)
    }
  }
}