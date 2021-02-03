/*
 *  Random.scala
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

import de.sciss.lucre.Adjunct.{HasDefault, Num, NumDouble}
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.AbstractRand
import de.sciss.lucre.expr.{Context, IAction, IControl}
import de.sciss.lucre.impl.RandomImpl
import de.sciss.lucre.{Adjunct, IExpr, IPull, ITargets, ProductWithAdjuncts, Txn, Random => LRandom}

import scala.concurrent.stm.Ref

object Random extends ProductReader[Random] {
  def apply(seed: Ex[Long] = TimeStamp()): Random = Impl(seed)

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Random = {
    require (arity == 1 && adj == 0)
    val _seed = in.readEx[Long]()
    Random(_seed)
  }

  // ---- Coin ----

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

    override def productPrefix: String = s"Random$$Coin"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, B] with IAction[T]

    /** Convenient method that returns the `Coin` itself. This can be used
      * for additional clarity when writing `Bang() ---> coin.update` instead of `Bang() ---> coin`.
      */
    def update: Act = this

    override def adjuncts: List[Adjunct] = num :: default :: Nil

    override protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Coin.Expanded[T, A, B](prob.expand[T] /*, tx*/)(ctx.targets, num, default, gen.expand[T])
  }

  // ---- Until ----

  object Until extends ProductReader[Until[_]] {
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

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Until[_] = {
      require (arity == 2 && adj == 1)
      val _hi   = in.readEx[Any]()
      val _gen  = in.readProductT[Random]()
      implicit val _num: Num[Any] = in.readAdjunct()
      new Until(_hi, _gen)
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
    *   val r100 = gen.until(100)
    *   Act(
    *     r100, // draw new number
    *     PrintLn("Random number: " ++ r100.toStr)  // print current value
    *   )
    * }}}
    */
  final case class Until[A](hi: Ex[A], gen: Random)(implicit num: Num[A])
    extends Ex[A] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Random$$Until"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, A] with IAction[T]

    /** Convenient method that returns the `Until` itself. This can be used
      * for additional clarity when writing `Bang() ---> x.update` instead of `Bang() ---> x`.
      */
    def update: Act = this

    override def adjuncts: List[Adjunct] = num :: Nil

    override protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Until.Expanded[T, A](hi.expand[T] /*, tx*/)(ctx.targets, num, gen.expand[T])
  }

  // ---- Range ----


  object Range extends ProductReader[Range[_]] {
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

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Range[_] = {
      require (arity == 3 && adj == 1)
      val _lo   = in.readEx[Any]()
      val _hi   = in.readEx[Any]()
      val _gen  = in.readProductT[Random]()
      implicit val _num: Num[Any] = in.readAdjunct()
      new Range(_lo, _hi, _gen)
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
    *   val r1to10 = gen.range(1, 10)
    *   Act(
    *     r1to10, // draw new number
    *     PrintLn("Random number (1 to 10): " ++ r1to10.toStr)  // print current value
    *   )
    * }}}
    */
  final case class Range[A](lo: Ex[A], hi: Ex[A], gen: Random)(implicit num: Num[A])
    extends Ex[A] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Random$$Range" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, A] with IAction[T]

    /** Convenient method that returns the `Range` itself. This can be used
      * for additional clarity when writing `Bang() ---> x.update` instead of `Bang() ---> x`.
      */
    def update: Act = this

    override def adjuncts: List[Adjunct] = num :: Nil

    override protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Range.Expanded[T, A](lo.expand[T], hi.expand[T], tx)(ctx.targets, num, gen.expand[T])
  }

  // ---- impl ----

  private final class Expanded[T <: Txn[T]](seed: IExpr[T, Long])
    extends RandomImpl.BaseImpl[T] with IControl[T] with LRandom[T] {

    private[this] val seedRef = Ref.make[Long]()

    override def initControl()(implicit tx: T): Unit =
      setSeed(seed.value)

    override def rawSeed_=(seed: Long)(implicit tx: T): Unit =
      seedRef() = seed

    override def rawSeed(implicit tx: T): Long =
      seedRef()

    override def dispose()(implicit tx: T): Unit = ()
  }

  private final case class Impl(seed: Ex[Long]) extends Random {
    override def productPrefix: String = "Random"   // serialization

    type Repr[T <: Txn[T]] = IControl[T] with LRandom[T]

    override protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Expanded[T](seed.expand[T])
  }
}
/** A random number generator that can be used for operations such as `.until` and `.range`.
  *
  * Example:
  *
  * {{{
  *   val gen = Random()
  *   val r100 = gen.until(100)
  *   Act(
  *     r100, // draw new number between 0 and 99
  *     PrintLn("Random number: " ++ r100.toStr)  // print current value
  *   )
  * }}}
  */
trait Random extends Control {
  type Repr[T <: Txn[T]] <: IControl[T] with LRandom[T]

  def until[A](hi: Ex[A])(implicit num: Num[A]): Ex[A] with Act = Random.Until(hi, this)

//  def rand2[A](hi: Ex[A])(implicit num: Num[A]): Ex[A] with Act = Rand2(hi, this)

  def range[A](lo: Ex[A], hi: Ex[A])(implicit num: Num[A]): Ex[A] with Act = Random.Range(lo, hi, this)

  def coin[A, B](prob: Ex[A])
                (implicit num: NumDouble[A] { type Boolean = B }, default: HasDefault[B]): Ex[num.Boolean] with Act =
    Random.Coin(prob, this)
}