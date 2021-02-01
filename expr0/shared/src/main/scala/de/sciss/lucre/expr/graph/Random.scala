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

import de.sciss.lucre.Adjunct.Num
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.impl.RandomImpl
import de.sciss.lucre.{IExpr, Txn, Random => LRandom}

import scala.concurrent.stm.Ref

object Random extends ProductReader[Random] {
  def apply(seed: Ex[Long] = TimeStamp()): Random = Impl(seed)

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Random = {
    require (arity == 1 && adj == 0)
    val _seed = in.readEx[Long]()
    Random(_seed)
  }

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
/** A random number generator that can be used for operations such as `.rand`.
  * Typically one will create a generator, and from it one or several random numbers:
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
trait Random extends Control {
  type Repr[T <: Txn[T]] <: IControl[T] with LRandom[T]

  def rand[A](hi: Ex[A])(implicit num: Num[A]): Ex[A] with Act = Rand(hi, this)
}