/*
 *  UnaryOp.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.aux.Aux.{Num, NumBool, NumFrac, NumInt, ToNum, Widen, WidenToDouble}
import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.UnaryOp.Op
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.model.Change

import scala.collection.immutable.{Seq => ISeq}

object UnaryOp {
  sealed abstract class Op[A1, A2] extends ProductWithAux {
    override final def productPrefix = s"UnaryOp$$$name"

    def name: String

    def apply(a: A1): A2

    override def toString: String = name
  }

  // ---- analogous to UGens ----

  final case class Neg[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.negate(a)
    def name                  : String    = "Neg"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Not[A]()(implicit num: NumBool[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.not(a)
    def name                  : String    = "Not"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class BitNot[A]()(implicit num: NumInt[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.unary_~(a)
    def name                  : String    = "BitNot"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.abs(a)
    def name                  : String    = "Abs"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class ToDouble[A, B]()(implicit to: ToNum[A] { type Double = B }) extends Op[A, B] {
    def apply(a: A)           : B         = to.toDouble(a)
    def name                  : String    = "ToDouble"
    def aux : scala.List[Aux] = to :: Nil
  }

  final case class ToInt[A, B]()(implicit to: ToNum[A] { type Int = B }) extends Op[A, B] {
    def apply(a: A)           : B         = to.toInt(a)
    def name                  : String    = "ToInt"
    def aux : scala.List[Aux] = to :: Nil
  }

  final case class Ceil[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.ceil(a)
    def name                  : String    = "Ceil"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Floor[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.floor(a)
    def name                  : String    = "Floor"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Frac[A]()(implicit num: NumFrac[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.frac(a)
    def name                  : String    = "Frac"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Signum[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.signum(a)
    def name                  : String    = "Signum"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Squared[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.squared(a)
    def name                  : String    = "Squared"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Cubed[A]()(implicit num: Num[A]) extends Op[A, A] {
    def apply(a: A)           : A         = num.cubed(a)
    def name                  : String    = "Cubed"
    def aux : scala.List[Aux] = num :: Nil
  }

  final case class Sqrt[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.sqrt(wd.widen1(a))
    def name                  : String    = "Sqrt"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Exp[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.exp(wd.widen1(a))
    def name                  : String    = "Exp"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Reciprocal[A, B]()(implicit w: Widen[A, B], num: NumFrac[B]) extends Op[A, B] {
    def apply(a: A )          : B         = num.reciprocal(w.widen1(a))
    def name                  : String    = "Reciprocal"
    def aux : scala.List[Aux] = w :: num :: Nil
  }

  final case class Midicps[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.midiCps(wd.widen1(a))
    def name                  : String    = "Midicps"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Cpsmidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.cpsMidi(wd.widen1(a))
    def name                  : String    = "Cpsmidi"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Midiratio[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.midiRatio(wd.widen1(a))
    def name                  : String    = "Midiratio"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Ratiomidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.ratioMidi(wd.widen1(a))
    def name                  : String    = "Ratiomidi"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Dbamp[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.dbAmp(wd.widen1(a))
    def name                  : String    = "Dbamp"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Ampdb[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.ampDb(wd.widen1(a))
    def name                  : String    = "Ampdb"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Octcps[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.octCps(wd.widen1(a))
    def name                  : String    = "Octcps"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Cpsoct[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.cpsOct(wd.widen1(a))
    def name                  : String    = "Cpsoct"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Log[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.log(wd.widen1(a))
    def name                  : String    = "Log"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Log2[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.log2(wd.widen1(a))
    def name = "Log2"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Log10[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.log10(wd.widen1(a))
    def name = "Log10"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Sin[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.sin(wd.widen1(a))
    def name = "Sin"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Cos[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.cos(wd.widen1(a))
    def name = "Cos"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Tan[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.tan(wd.widen1(a))
    def name = "Tan"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Asin[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.asin(wd.widen1(a))
    def name = "Asin"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Acos[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.acos(wd.widen1(a))
    def name = "Acos"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Atan[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A): B         = wd.atan(wd.widen1(a))
    def name = "Atan"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Sinh[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.sinh(wd.widen1(a))
    def name                  : String    = "Sinh"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Cosh[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.cosh(wd.widen1(a))
    def name                  : String    = "Cosh"
    def aux : scala.List[Aux] = wd :: Nil
  }

  final case class Tanh[A, B]()(implicit wd: WidenToDouble[A, B]) extends Op[A, B] {
    def apply(a: A)           : B         = wd.tanh(wd.widen1(a))
    def name                  : String    = "Tanh"
    def aux : scala.List[Aux] = wd :: Nil
  }

//  final case class Rand[A]()(implicit num: Num[A])

//  final case class Rand2

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand

  // Distort
  // Softclip

//  final case class Coin[A, B]()(implicit num: NumDouble[A] { type Boolean = B })

  // RectWindow
  // HanWindow
  // WelWindow
  // TriWindow

  // Ramp
  // Scurve

  // ---- general ----

  final case class ToStr[A]() extends Op[A, String] {
    def apply(a: A)           : String  = a.toString
    def name                  : String  = "ToStr"
    def aux : scala.List[Aux] = Nil
  }

  // ---- Option ----

  final case class OptionSome[A]() extends Op[A, Option[A]] {
    def apply(a: A)           : Option[A] = Some(a)
    def name                  : String    = "OptionSome"
    def aux : scala.List[Aux] = Nil
  }

  final case class OptionIsEmpty[A]() extends Op[Option[A], Boolean] {
    def apply(a: Option[A])   : Boolean = a.isEmpty
    def name                  : String  = "OptionIsEmpty"
    def aux : scala.List[Aux] = Nil
  }

  final case class OptionIsDefined[A]() extends Op[Option[A], Boolean] {
    def apply(a: Option[A])   : Boolean = a.isDefined
    def name                  : String  = "OptionIsDefined"
    def aux : scala.List[Aux] = Nil
  }

  final case class OptionToList[A]() extends Op[Option[A], scala.List[A]] {
    def apply(a: Option[A])   : scala.List[A] = a.toList
    def name                  : String  = "OptionToList"
    def aux : scala.List[Aux] = Nil
  }

  // ---- Seq ----

  final case class SeqSize[A]() extends Op[ISeq[A], Int] {
    def apply(a: ISeq[A])     : Int     = a.size
    def name                  : String  = "SeqSize"
    def aux : scala.List[Aux] = Nil
  }

  final case class SeqHeadOption[A]() extends Op[ISeq[A], Option[A]] {
    def apply(a: ISeq[A])     : Option[A] = a.headOption
    def name                  : String    = "SeqHeadOption"
    def aux : scala.List[Aux] = Nil
  }

  final case class SeqLastOption[A]() extends Op[ISeq[A], Option[A]] {
    def apply(a: ISeq[A])     : Option[A] = a.lastOption
    def name                  : String    = "SeqLastOption"
    def aux : scala.List[Aux] = Nil
  }

  final case class SeqIsEmpty[A]() extends Op[ISeq[A], Boolean] {
    def apply(a: ISeq[A])     : Boolean = a.isEmpty
    def name                  : String  = "SeqIsEmpty"
    def aux : scala.List[Aux] = Nil
  }

  final case class SeqNonEmpty[A]() extends Op[ISeq[A], Boolean] {
    def apply(a: ISeq[A])     : Boolean = a.nonEmpty
    def name                  : String  = "SeqNonEmpty"
    def aux : scala.List[Aux] = Nil
  }

  private final class Expanded[S <: Base[S], A1, A](op: Op[A1, A], a: IExpr[S, A1], tx0: S#Tx)
                                                   (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IEventImpl[S, Change[A]] {

    a.changed.--->(this)(tx0)

    override def toString: String = s"UnaryOp($op, $a)"

    def changed: IEvent[S, Change[A]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
      pull(a.changed).flatMap { ach =>
        val before  = value1(ach.before)
        val now     = value1(ach.now   )
        if (before == now) None else Some(Change(before, now))
      }
    }

    @inline
    private def value1(av: A1): A = op(av)

    def value(implicit tx: S#Tx): A = {
      val av = a.value
      value1(av)
    }

    def dispose()(implicit tx: S#Tx): Unit =
      a.changed -/-> this
  }
}

final case class UnaryOp[A1, A](op: Op[A1, A], a: Ex[A1])
  extends Ex.Lazy[A] {

  protected def mkExpr[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] = {
    import ctx.targets
    val ax = a.expand[S]
    new UnaryOp.Expanded[S, A1, A](op, ax, tx)
  }
}
