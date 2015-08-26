/*
 *  CopyImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm
package impl

import scala.collection.mutable

final class CopyImpl[S <: Sys[S]](implicit tx: S#Tx) extends Copy[S] {
  private[this] sealed trait State
  private[this] final case class Done(elem: Elem[S]) extends State
  private[this] case object Busy extends State

  private[this] val stateMap = mutable.Map.empty[Elem[S], State]
  private[this] val hintMap  = mutable.Map.empty[Elem[S], mutable.Map[String, Any]]

  def provide[Repr <: Obj[S]](in: Repr, out: Repr): Unit = {
    if (!stateMap.get(in).contains(Busy))
      throw new IllegalStateException(s"Copy.provide must be called during copy process: $in")
    stateMap.put(in, Done(out))
  }

  private def perform[Repr <: Elem[S]](in: Repr): Repr =
    stateMap.get(in) match {
      case Some(Done(out)) => out.asInstanceOf[Repr]
      case Some(Busy) => throw new IllegalStateException(s"Cyclic object graph involving $in")
      case None =>
        stateMap.put(in, Busy)
        val out = in.copy()(tx, this)
        stateMap.put(in, Done(out))
        out.asInstanceOf[Repr]
    }

  def putHint[Repr <: Elem[S]](in: Repr, key: String, value: Any): Unit = {
    val map = hintMap.getOrElse(in, {
      val res = mutable.Map.empty[String, Any]
      hintMap.put(in, res)
      res
    })
    map.put(key, value)
  }

  def getHint[Repr <: Elem[S]](in: Repr, key: String): Option[Any] =
    hintMap.get(in).flatMap(_.get(key))

  def apply[Repr <: Elem[S]](in: Repr): Repr = {
    val out = perform(in)
    (in, out) match {
      case (inObj: Obj[S], outObj: Obj[S]) => // copy the attributes
        val inAttr  = inObj .attr
        val outAttr = outObj.attr
        inAttr.iterator.foreach { case (key, value) =>
          outAttr.put(key, apply(value))
        }
      case _ =>
    }
    out
  }
}
