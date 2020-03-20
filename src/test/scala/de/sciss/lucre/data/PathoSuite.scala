package de.sciss.lucre.data

import de.sciss.lucre.geom.{IntPoint2D, IntSpace, IntSquare}
import de.sciss.lucre.stm.InMemory
import org.scalatest.funspec.AnyFunSpec

/*
 To run only this suite

test-only de.sciss.lucre.data.PathoSuite

  */
class PathoSuite extends AnyFunSpec {
  type S  = InMemory
  type D  = IntSpace.TwoDim
  implicit val view: (IntPoint2D, S#Tx) => IntPoint2D = (p, _) => p
  val i: S = InMemory()
  val j   = 8

  i.step { implicit tx =>
    val cube  = IntSquare(j >> 1, j >> 1, j >> 1)
    val tree  = SkipOctree.empty[S, D, IntPoint2D](cube)
    val ins   = (0 until j).map { i => IntPoint2D(j >> 1, i) }
    // val v     = IntPoint2D(j - 1, 0)

    //val ins   = Vector.fill(j)(IntPoint2D(r.nextInt(j), r.nextInt(j)))
    ins.foreach(tree += _)
    // tree.nearestNeighbor(v, IntDistanceMeasure2D.euclideanSq)
  }
}
