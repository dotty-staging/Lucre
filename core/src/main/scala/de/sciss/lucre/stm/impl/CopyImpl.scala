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

  private[this] val idMap         = tx.newInMemoryIDMap[State]
  private[this] val constMap      = mutable.Map.empty[Elem[S], State]
  private[this] val idHintMap     = tx.newInMemoryIDMap[mutable.Map[String, Any]]
  private[this] val constHintMap  = mutable.Map.empty[Elem[S], mutable.Map[String, Any]]

  def provide[Repr <: Obj[S]](in: Repr, out: Repr): Unit = {
    val id = in.id
    if (!idMap.get(id).contains(Busy))
      throw new IllegalStateException(s"Copy.provide must be called during copy process: $in")
    idMap.put(id, Done(out))
  }

  private def perform[Repr <: Elem[S]](in: Repr, state: Option[State], put: State => Unit): Repr = state match {
    case Some(Done(out)) => out.asInstanceOf[Repr]
    case Some(Busy) => throw new IllegalStateException(s"Cyclic object graph involving $in")
    case None =>
      put(Busy)
      val out = in.copy()(tx, this)
      put(Done(out))
      out.asInstanceOf[Repr]
  }

  def putHint[Repr <: Elem[S]](in: Repr, key: String, value: Any): Unit = {
    val map = in match {
      case obj: Identifiable[_] =>
        val id = obj.id.asInstanceOf[S#ID]
        idHintMap.getOrElse(id, { val res = mutable.Map.empty[String, Any]; idHintMap.put(id, res); res })
      case _ =>
        constHintMap.getOrElse(in, { val res = mutable.Map.empty[String, Any]; constHintMap.put(in, res); res })
    }
    map.put(key, value)
  }

  def getHint[Repr <: Elem[S]](in: Repr, key: String): Option[Any] = in match {
    case obj: Identifiable[_] =>
      val id = obj.id.asInstanceOf[S#ID]
      idHintMap.get(id).flatMap(_.get(key))
    case _ =>
      constHintMap.get(in).flatMap(_.get(key))
  }

  def apply[Repr <: Elem[S]](in: Repr): Repr = in match {
    case obj: Obj[S] =>
      val id      = obj.id
      val res     = perform(in, idMap.get(id), idMap.put(id, _))
      // now copy the attributes
      val inAttr  = obj.attr
      val outAttr = res.asInstanceOf[Obj[S]].attr   // Scala does not infer that `obj` must also be `Repr`
      inAttr.iterator.foreach { case (key, value) =>
        outAttr.put(key, apply(value))
      }
      res
    case obj: Identifiable[_] =>
      val id = obj.id.asInstanceOf[S#ID]
      perform(in, idMap.get(id), idMap.put(id, _))
    case _ =>
      perform(in, constMap.get(in), constMap.put(in, _))
  }
}
