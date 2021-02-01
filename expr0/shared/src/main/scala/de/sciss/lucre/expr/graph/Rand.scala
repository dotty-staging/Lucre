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

object Rand extends ProductReader[Rand[_]] {
  private final class Expanded[T <: Txn[T], A](hi: IExpr[T, A] /*, tx0: T*/)
                                              (implicit protected val targets: ITargets[T], num: Num[A], gen: LRandom[T])
    extends AbstractRand[T, A](num.zero) {

//    hi.changed.--->(this)(tx0)

    override protected def mkNewValue()(implicit tx: T): A = {
      val hiVal = hi.value
      num.rand[T](hiVal)
    }

    override protected def pullNewValue(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): A = {
      val hiVal = pull.expr(hi)
      num.rand[T](hiVal)
    }

//    override def dispose()(implicit tx: T): Unit = {
//      super.dispose()
//      hi.changed.-/->(this)
//    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Rand[_] = {
    require (arity == 2 && adj == 1)
    val _hi   = in.readEx[Any]()
    val _gen  = in.readProductT[Random]()
    implicit val _num: Num[Any] = in.readAdjunct()
    new Rand(_hi, _gen)
  }
}
/** A random number between zero (inclusive) and a given `hi` boundary (exclusive).
  * `hi` may be negative, but it must not be zero.
  *
  * This is both an expression and an action. The action draws a new random number,
  * the expression reports the last drawn value.
  *
  * Example:
  *
  * {{{
  *   val gen = Random()
  *   val r100 = gen.rand(100)
  *   Act(
  *     r100, // draw new number
  *     PrintLn("Random number: " ++ r100.toStr)  // print current value
  *   )
  * }}}
  */
final case class Rand[A](hi: Ex[A], gen: Random)(implicit num: Num[A])
  extends Ex[A] with Act with ProductWithAdjuncts {

  type Repr[T <: Txn[T]] = IExpr[T, A] with IAction[T]

  override def adjuncts: List[Adjunct] = num :: Nil

  override protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new Rand.Expanded[T, A](hi.expand[T] /*, tx*/)(ctx.targets, num, gen.expand[T])
}
