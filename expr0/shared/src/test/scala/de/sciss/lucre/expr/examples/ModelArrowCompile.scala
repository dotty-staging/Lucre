package de.sciss.lucre.expr.examples

import de.sciss.lucre.expr.Model
import de.sciss.lucre.expr.graph.{Attr, Ex, Var}

trait ModelArrowCompile[A] {
  def m: Model[A]
  def n: Model[A]

  def a: Attr[A]

  def ad: Attr.WithDefault[A]

  def vr: Var[A]

  def x: Ex[A]

  m <-> ad
  m --> ad
  m <-- ad
  m --> a
  m <-> vr
  m --> vr
  m <-- vr
  m <-- x
  m <-> n
  m --> n
  m <-- n
  x --> m
}
