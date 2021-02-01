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
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, IChangeEvent, IExpr, IPull, IPush, ITargets, ProductWithAdjuncts, Txn, Random => LRandom}
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object Rand extends ProductReader[Rand[_]] {
  private final class Expanded[T <: Txn[T], A](hi: IExpr[T, A], tx0: T)
                                              (implicit protected val targets: ITargets[T], num: Num[A], gen: LRandom[T])
    extends IChangeGeneratorEvent[T, A] with IExpr[T, A] with IActionImpl[T] {

    private[this] val ref = Ref(num.zero)

    hi.changed.--->(this)(tx0)

    override def value(implicit tx: T): A =
      IPush.tryPull(this).fold(ref())(_.now)

    override private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase) : A =
      if (pull.isOrigin(this)) pull.resolveChange[A]
      else if (phase.isBefore) ref() else {
        val hiVal = pull.expr(hi)
        mkNewValue(hiVal)
      }

    private def mkNewValue(hiVal: A)(implicit tx: T): A = {
      val now = num.rand[T](hiVal)
      ref() = now
      now
    }

    override def changed: IChangeEvent[T, A] = this

    override def dispose()(implicit tx: T): Unit = {
      super.dispose()
      hi.changed.-/->(this)
    }

    override def executeAction()(implicit tx: T): Unit = {
      val before  = ref()
      val now     = mkNewValue(hi.value)
      if (before != now) fire(Change(before, now))
    }
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
    new Rand.Expanded[T, A](hi.expand[T], tx)(ctx.targets, num, gen.expand[T])
}
