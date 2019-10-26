package de.sciss.lucre.expr

import scala.language.{higherKinds, implicitConversions}

trait NewExActMapBlueprint {
  trait Ex[+A] {
    def value: A
  }

  trait Act {
    def execute(): Unit
  }

  def PrintLn(in: Ex[String]): Act

  def aOpt: Ex[Option[String]]
  def bOpt: Ex[Option[String]]

  def aSeq: Ex[Seq[String]]
  def bSeq: Ex[Seq[String]]

  def LoadBang(act: Act): Unit

  trait CanFlatMap[-From[_], -B, +To] {
    def flatMap[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  trait CanMap[-From[_], -B, +To] {
    def map[A](from: Ex[From[A]], fun: Ex[A] => B): To
  }

  implicit def ExOptionOps[A] (in: Ex[Option[A]]) : ExOptionOps [A]
  implicit def ExSeqOps   [A] (in: Ex[Seq   [A]]) : ExSeqOps    [A]
  implicit def ExStringOps    (in: Ex[String])    : ExStringOps

  implicit def canMapExOptionToAct    : CanMap     [Option, Act, Act]
  implicit def canFlatMapExOptionToAct: CanFlatMap [Option, Act, Act]
  implicit def canMapExSeqToAct       : CanMap     [Seq   , Act, Act]
  implicit def canFlatMapExSeqToAct   : CanFlatMap [Seq   , Act, Act]

  trait CanMapOptionActAct extends CanMap[Option, Act, Act] {
    def map[A](from: Ex[Option[A]], fun: Ex[A] => Act): Act   // seems doable
  }

  trait CanFlatMapOptionActAct extends CanFlatMap[Option, Act, Act] {
    def flatMap[A](from: Ex[Option[A]], fun: Ex[A] => Act): Act   // seems doable
  }

  trait CanMapSeqActAct extends CanMap[Seq, Act, Act] {
    def map[A](from: Ex[Seq[A]], fun: Ex[A] => Act): Act   // seems doable
  }

  trait CanFlatMapSeqActAct extends CanFlatMap[Seq, Act, Act] {
    def flatMap[A](from: Ex[Seq[A]], fun: Ex[A] => Act): Act   // seems doable
  }

  abstract class ExOptionOps[A](x: Ex[Option[A]]) {
    def flatMap[B, To](f: Ex[A] => B)(implicit fm: CanFlatMap[Option, B, To]): To =
      fm.flatMap(x, f)

    def map[B, To](f: Ex[A] => B)(implicit m: CanMap[Option, B, To]): To =
      m.map(x, f)

    def withFilter(f: Ex[A] => Ex[Boolean]): Ex[Option[A]]  // seems doable
  }

  abstract class ExSeqOps[A](x: Ex[Seq[A]]) {
    def flatMap[B, To](f: Ex[A] => B)(implicit fm: CanFlatMap[Seq, B, To]): To =
      fm.flatMap(x, f)

    def map[B, To](f: Ex[A] => B)(implicit m: CanMap[Seq, B, To]): To =
      m.map(x, f)

    def withFilter(f: Ex[A] => Ex[Boolean]): Ex[Seq[A]]  // seems doable
  }

  trait ExStringOps {
    def nonEmpty: Ex[Boolean]

    def ++(b: Ex[String]): Ex[String]
  }

  def testOptions(): Unit = {
    val actFromOpt = for {
      a <- aOpt
      b <- bOpt
    } yield {
      PrintLn(a ++ b)
    }

    val actFromOptGuard = for {
      a <- aOpt
      b <- bOpt
      if b.nonEmpty
    } yield {
      PrintLn(a ++ b)
    }

    LoadBang(actFromOpt)
    LoadBang(actFromOptGuard)
  }

  def testSeq(): Unit = {
    val actFromSeq = for {
      a <- aSeq
      b <- bSeq
    } yield {
      PrintLn(a ++ b)
    }

    val actFromSeqGuard = for {
      a <- aSeq
      b <- bSeq
      if b.nonEmpty
    } yield {
      PrintLn(a ++ b)
    }

    LoadBang(actFromSeq)
    LoadBang(actFromSeqGuard)
  }
}
