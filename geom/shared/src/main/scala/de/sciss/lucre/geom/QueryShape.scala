/*
 *  QueryShape.scala
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

/** A shape for range queries. Type `A` indicates the results
  * of area calculations, and may be specialized.
  */
trait QueryShape[Area, P, H] {
  def overlapArea(q: H): Area

  def isAreaGreater(a: H, b: Area): Boolean

  def isAreaNonEmpty(area: Area): Boolean

  /** Queries the overlap of this shape with a given
    * `IntPoint2D p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
  def containsP(p: P): Boolean
}
