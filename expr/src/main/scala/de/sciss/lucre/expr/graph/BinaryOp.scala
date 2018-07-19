package de.sciss.lucre.expr
package graph

import de.sciss.lucre.aux.Aux.Widen2
import de.sciss.lucre.stm.Base
import de.sciss.lucre.aux.{Aux, BinaryOp => Op}

final case class BinaryOp[A1, A2, A3, A](op: Op[A3, A], a: Ex[A1], b: Ex[A2])
                                        (implicit val widen: Widen2[A1, A2, A3])
  extends Ex[A] { pat =>

  def aux: scala.List[Aux] = widen :: Nil

  def value[S <: Base[S]](implicit tx: S#Tx): A = {
    val av = a.value[S]
    val bv = b.value[S]
    ???
  }

//  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
//    val aT = t(a)
//    val bT = t(b)
//    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
//  }
}
