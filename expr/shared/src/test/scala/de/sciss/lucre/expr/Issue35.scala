package de.sciss.lucre.expr

import graph._

trait Issue35 {
  def ai: Ex[Int]
  def ad: Ex[Double]
  def bi: Ex[Int]
  def bd: Ex[Double]

  def ci: Ex[Int] = ai / bi
  def cd: Ex[Double] = ad / bd
  def dd: Ex[Double] = ai / bd
  def ed: Ex[Double] = ad / bi

  def ci_inferred = ai / bi
  def di_inferred = ai / 123
  def ed_inferred = ai / bd
}
trait Issue35Ext {
  def peer: Issue35

  def isInt1: Ex[Int] = peer.ci_inferred
  def isInt2: Ex[Int] = peer.di_inferred
  def isDouble1: Ex[Double] = peer.ed_inferred
}
