/*
 *  Rand.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Adjunct.Num
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.AbstractRand
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.{Adjunct, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn, Random => LRandom}

object RangeRand extends ProductReader[RangeRand[_]] {
  private final class Expanded[T <: Txn[T], A](lo: IExpr[T, A], hi: IExpr[T, A] , tx0: T)
                                              (implicit protected val targets: ITargets[T], num: Num[A], gen: LRandom[T])
    extends AbstractRand[T, A](num.clip(num.zero, lo.value(tx0), hi.value(tx0)) /* num.zero*/) {

//    lo.changed.--->(this)(tx0)
//    hi.changed.--->(this)(tx0)

    override protected def mkNewValue()(implicit tx: T): A = {
      val loVal = lo.value
      val hiVal = hi.value
      num.rangeRand[T](loVal, hiVal)
    }

    override protected def pullNewValue(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val loVal = pull.expr(lo)
      val hiVal = pull.expr(hi)
      num.rangeRand[T](loVal, hiVal)
    }

//    override def dispose()(implicit tx: T): Unit = {
//      super.dispose()
//      lo.changed.-/->(this)
//      hi.changed.-/->(this)
//    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RangeRand[_] = {
    require (arity == 3 && adj == 1)
    val _lo   = in.readEx[Any]()
    val _hi   = in.readEx[Any]()
    val _gen  = in.readProductT[Random]()
    implicit val _num: Num[Any] = in.readAdjunct()
    new RangeRand(_lo, _hi, _gen)
  }
}
/** A random number between a given `lo` and a given `hi` boundary.
  * The boundaries are inclusive
  * for integer numbers. For floating point numbers, the `hi` bound is exclusive.
  *
  * This is both an expression and an action. The action draws a new random number,
  * the expression reports the last drawn value.
  *
  * Example:
  *
  * {{{
  *   val gen = Random()
  *   val r1to10 = gen.rangeRand(1, 10)
  *   Act(
  *     r1to10, // draw new number
  *     PrintLn("Random number (1 to 10): " ++ r1to10.toStr)  // print current value
  *   )
  * }}}
  */
final case class RangeRand[A](lo: Ex[A], hi: Ex[A], gen: Random)(implicit num: Num[A])
  extends Ex[A] with Act with ProductWithAdjuncts {

  type Repr[T <: Txn[T]] = IExpr[T, A] with IAction[T]

  override def adjuncts: List[Adjunct] = num :: Nil

  override protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new RangeRand.Expanded[T, A](lo.expand[T], hi.expand[T], tx)(ctx.targets, num, gen.expand[T])
}
