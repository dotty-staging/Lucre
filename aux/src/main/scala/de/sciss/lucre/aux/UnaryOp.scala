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

package de.sciss.lucre.aux

import de.sciss.lucre.aux.Aux.{Num, NumBool, NumDouble, NumFrac, NumInt, ToNum, Widen, WidenToDouble}
import de.sciss.lucre.stm.{Base, TxnRandom}
import de.sciss.serial.{DataInput, DataOutput}

import scala.language.higherKinds

object UnaryOp {
  abstract class Pure[A1, A2] extends UnaryOp[A1, A2] {
    final type State[S <: Base[S]] = Unit

    final def readState   [S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S] = ()
    final def writeState  [S <: Base[S]](s: State[S], out: DataOutput): Unit = ()
    final def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit = ()

    final def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S] = ()

    def next[S <: Base[S]](a: A1)(implicit state: State[S], tx: S#Tx): A2 = apply(a)

    def apply(a: A1): A2
  }

  abstract class RandomOp[A1, A2] extends UnaryOp[A1, A2] {
    final type State[S <: Base[S]] = TxnRandom[S]

    final def readState[S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S] =
      TxnRandom.read(in, access)

    final def writeState[S <: Base[S]](s: State[S], out: DataOutput): Unit =
      s.write(out)

    final def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit =
      s.dispose()

    final def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S] = ctx.mkRandom(ref)
  }

  // ---- analogous to UGens ----

  final case class Neg[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.negate(a)
    def name                  : String    = "Neg"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Not[A]()(implicit num: NumBool[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.not(a)
    def name                  : String    = "Not"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class BitNot[A]()(implicit num: NumInt[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.unary_~(a)
    def name                  : String    = "BitNot"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Abs[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.abs(a)
    def name                  : String    = "Abs"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class ToDouble[A, B]()(implicit to: ToNum[A] { type Double = B }) extends Pure[A, B] {
    def apply(a: A)           : B         = to.toDouble(a)
    def name                  : String    = "ToDouble"
    def aux                   : List[Aux] = to :: Nil
  }

  final case class ToInt[A, B]()(implicit to: ToNum[A] { type Int = B }) extends Pure[A, B] {
    def apply(a: A)           : B         = to.toInt(a)
    def name                  : String    = "ToInt"
    def aux                   : List[Aux] = to :: Nil
  }

  final case class Ceil[A]()(implicit num: NumFrac[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.ceil(a)
    def name                  : String    = "Ceil"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Floor[A]()(implicit num: NumFrac[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.floor(a)
    def name                  : String    = "Floor"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Frac[A]()(implicit num: NumFrac[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.frac(a)
    def name                  : String    = "Frac"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Signum[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.signum(a)
    def name                  : String    = "Signum"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Squared[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.squared(a)
    def name                  : String    = "Squared"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Cubed[A]()(implicit num: Num[A]) extends Pure[A, A] {
    def apply(a: A)           : A         = num.cubed(a)
    def name                  : String    = "Cubed"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Sqrt[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.sqrt(wd.widen1(a))
    def name                  : String    = "Sqrt"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Exp[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.exp(wd.widen1(a))
    def name                  : String    = "Exp"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Reciprocal[A, B]()(implicit w: Widen[A, B], num: NumFrac[B]) extends Pure[A, B] {
    def apply(a: A )          : B         = num.reciprocal(w.widen1(a))
    def name                  : String    = "Reciprocal"
    def aux                   : List[Aux] = w :: num :: Nil
  }

  final case class Midicps[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.midiCps(wd.widen1(a))
    def name                  : String    = "Midicps"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Cpsmidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.cpsMidi(wd.widen1(a))
    def name                  : String    = "Cpsmidi"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Midiratio[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.midiRatio(wd.widen1(a))
    def name                  : String    = "Midiratio"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Ratiomidi[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.ratioMidi(wd.widen1(a))
    def name                  : String    = "Ratiomidi"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Dbamp[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.dbAmp(wd.widen1(a))
    def name                  : String    = "Dbamp"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Ampdb[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.ampDb(wd.widen1(a))
    def name                  : String    = "Ampdb"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Octcps[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.octCps(wd.widen1(a))
    def name                  : String    = "Octcps"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Cpsoct[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.cpsOct(wd.widen1(a))
    def name                  : String    = "Cpsoct"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Log[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.log(wd.widen1(a))
    def name                  : String    = "Log"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Log2[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.log2(wd.widen1(a))
    def name                  : String    = "Log2"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Log10[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.log10(wd.widen1(a))
    def name                  : String    = "Log10"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Sin[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.sin(wd.widen1(a))
    def name                  : String    = "Sin"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Cos[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.cos(wd.widen1(a))
    def name                  : String    = "Cos"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Tan[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.tan(wd.widen1(a))
    def name                  : String    = "Tan"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Asin[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A): B         = wd.asin(wd.widen1(a))
    def name = "Asin"
    def aux                  : List[Aux] = wd :: Nil
  }

  final case class Acos[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A): B         = wd.acos(wd.widen1(a))
    def name = "Acos"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Atan[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B                   = wd.atan(wd.widen1(a))
    def name                             = "Atan"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Sinh[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.sinh(wd.widen1(a))
    def name                  : String    = "Sinh"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Cosh[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.cosh(wd.widen1(a))
    def name                  : String    = "Cosh"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Tanh[A, B]()(implicit wd: WidenToDouble[A, B]) extends Pure[A, B] {
    def apply(a: A)           : B         = wd.tanh(wd.widen1(a))
    def name                  : String    = "Tanh"
    def aux                   : List[Aux] = wd :: Nil
  }

  final case class Rand[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[S <: Base[S]](a: A)(implicit state: TxnRandom[S], tx: S#Tx): A = num.rand(a)
    def name                  : String    = "Rand"
    def aux                   : List[Aux] = num :: Nil
  }

  final case class Rand2[A]()(implicit num: Num[A]) extends RandomOp[A, A] {
    def next[S <: Base[S]](a: A)(implicit state: TxnRandom[S], tx: S#Tx): A = num.rand2(a)
    def name                  : String    = "Rand2"
    def aux                   : List[Aux] = num :: Nil
  }

  // XXX TODO:
  // Linrand
  // Bilinrand
  // Sum3rand

  // Distort
  // Softclip

  final case class Coin[A, B]()(implicit num: NumDouble[A] { type Boolean = B }) extends RandomOp[A, B] {
    def next[S <: Base[S]](a: A)(implicit state: TxnRandom[S], tx: S#Tx): B = num.coin(a)
    def name                  : String    = "Coin"
    def aux                   : List[Aux] = num :: Nil
  }

  // RectWindow
  // HanWindow
  // WelWindow
  // TriWindow

  // Ramp
  // Scurve
}
sealed abstract class UnaryOp[A1, A2] extends ProductWithAux {
  type State[S <: Base[S]]

  //    protected def opId: Int

  def readState   [S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): State[S]
  def writeState  [S <: Base[S]](s: State[S], out: DataOutput): Unit
  def disposeState[S <: Base[S]](s: State[S])(implicit tx: S#Tx): Unit

  def prepare[S <: Base[S]](ref: AnyRef)(implicit ctx: Context[S], tx: S#Tx): State[S]

  def next[S <: Base[S]](a: A1)(implicit state: State[S], tx: S#Tx): A2

  override final def productPrefix = s"UnaryOp$$$name"

  def name: String

  override def toString: String = name
}
