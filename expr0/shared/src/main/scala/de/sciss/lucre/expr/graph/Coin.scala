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

import de.sciss.lucre.Adjunct.{HasDefault, NumDouble}
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.AbstractRand
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.{Adjunct, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn, Random => LRandom}

object Coin extends ProductReader[Coin[_, _]] {
  private final class Expanded[T <: Txn[T], A, B](prob: IExpr[T, A] /*, tx0: T*/)
                                              (implicit protected val targets: ITargets[T],
                                               num: NumDouble[A] { type Boolean = B },
                                               default: HasDefault[B], gen: LRandom[T])
    extends AbstractRand[T, B](default.defaultValue) {

//    prob.changed.--->(this)(tx0)

    override protected def mkNewValue()(implicit tx: T): B = {
      val probVal = prob.value
      num.coin[T](probVal)
    }

    override protected def pullNewValue(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): B = {
      val probVal = pull.expr(prob)
      num.coin[T](probVal)
    }

//    override def dispose()(implicit tx: T): Unit = {
//      super.dispose()
//      prob.changed.-/->(this)
//    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Coin[_, _] = {
    require (arity == 2 && adj == 2)
    val _prob = in.readEx[Any]()
    val _gen  = in.readProductT[Random]()
    implicit val _num     : NumDouble  [Any] { type Boolean = Any }  = in.readAdjunct()
    implicit val _default : HasDefault [Any]                         = in.readAdjunct()
    new Coin(_prob, _gen)
  }
}
/** A random boolean with a given probability. If `prob` is zero, the output will
  * always be `false`, if `prob` is one, the output will always be `true`, if
  * `prob` is `0.5`, the likelihood of `false` and `true` is equal.
  *
  * This is both an expression and an action. The action draws a new random number,
  * the expression reports the last drawn value.
  *
  * Example:
  *
  * {{{
  *   val gen = Random()
  *   val coin = gen.coin(0.4)
  *   Act(
  *     coin, // draw new number
  *     PrintLn("Random coin (slightly favouring false): " ++ coin.toStr)  // print current value
  *   )
  * }}}
  */
final case class Coin[A, B](prob: Ex[A], gen: Random)(implicit num: NumDouble[A] { type Boolean = B},
                                                      default: HasDefault[B])
  extends Ex[B] with Act with ProductWithAdjuncts {

  type Repr[T <: Txn[T]] = IExpr[T, B] with IAction[T]

  override def adjuncts: List[Adjunct] = num :: default :: Nil

  override protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new Coin.Expanded[T, A, B](prob.expand[T] /*, tx*/)(ctx.targets, num, default, gen.expand[T])
}
