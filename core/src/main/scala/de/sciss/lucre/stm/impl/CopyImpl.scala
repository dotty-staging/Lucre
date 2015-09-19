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
import scala.language.higherKinds
import scala.reflect.ClassTag

final class CopyImpl[In <: Sys[In], Out <: Sys[Out]](implicit txIn: In#Tx, txOut: Out#Tx)
  extends Copy[In, Out] {

  private[this] sealed trait State
  private[this] final case class Done(elem: Elem[Out]) extends State
  private[this] case object Busy extends State

  private[this] val stateMap = mutable.Map.empty[Elem[In], State]
  private[this] val hintMap  = mutable.Map.empty[Elem[In], mutable.Map[String, Any]]
  private[this] var deferred: mutable.Buffer[() => Unit] = _

  private def newPhase(): Unit =
    deferred = mutable.Buffer.empty[() => Unit]

  newPhase()

  def defer[Repr[~ <: Sys[~]] <: Obj[~]](in: Repr[In], out: Repr[Out])(code: => Unit): Unit = {
    if (stateMap.get(in) != Some(Busy))
      throw new IllegalStateException(s"Copy.provide must be called during copy process: $in")
    stateMap.put(in, Done(out))
    deferred += (() => code)
  }

  def finish(): Unit =
    while (deferred.nonEmpty) {
      val d = deferred
      newPhase()
      d.foreach(_.apply())
    }

  def apply[Repr[~ <: Sys[~]] <: Elem[~]](in: Repr[In]): Repr[Out] =
    stateMap.get(in) match {
      case Some(Done(out)) => out.asInstanceOf[Repr[Out]]
      case Some(Busy) => throw new IllegalStateException(s"Cyclic object graph involving $in")
      case None =>
        stateMap.put(in, Busy)
        val out = in.copy()(txIn, txOut, this)
        stateMap.put(in, Done(out))
        (in, out) match {
          case (inObj: Obj[In], outObj: Obj[Out]) => // copy the attributes
            copyAttr(inObj, outObj)
          case _ =>
        }
        out.asInstanceOf[Repr[Out]]
    }

  def putHint[A](in: Elem[In], key: String, value: A): Unit = {
    val map = hintMap.getOrElse(in, {
      val res = mutable.Map.empty[String, Any]
      hintMap.put(in, res)
      res
    })
    map.put(key, value)
  }

  def getHint[A](in: Elem[In], key: String)(implicit ct: ClassTag[A]): Option[A] =
    hintMap.get(in).flatMap(_.get(key) match {
      case a: A => Some(a)
      case _ => None
    })

  def copyAttr(in: Obj[In], out: Obj[Out]): Unit = {
    val inAttr  = in .attr
    val outAttr = out.attr
    inAttr.iterator.foreach { case (key, value) =>
      outAttr.put(key, apply(value))
    }
  }
}
