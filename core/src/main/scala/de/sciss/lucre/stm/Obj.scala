/*
 *  Obj.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import de.sciss.equal.Implicits._
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.{ObjImpl => Impl}
import de.sciss.serial.{DataInput, Serializer}

import scala.language.higherKinds

object Obj {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = Impl.read(in, access)

  /** Copy an object graph with `in` as a leaf.
    * This is short for the following sequence:
    *
    * {{{
    * val c   = Copy[Int, Out]
    * val out = c(in)
    * c.finish()
    * }}}
    */
  def copy[In <: Sys[In], Out <: Sys[Out], Repr[~ <: Base[~]] <: Elem[~]](in: Repr[In])
                                                                        (implicit txIn: In#Tx, txOut: Out#Tx): Repr[Out] = {
    val context = Copy[In, Out]
    val res     = context[Repr](in)
    context.finish()
    res
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Obj[S]] = Impl.serializer

  trait Type extends Elem.Type {
    private[this] lazy val _init: Unit = Obj.addType(this)

    override def init(): Unit = {
      super.init()
      _init
    }

    final override def readObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
      val tpe = in.readInt()
      if (tpe !== typeId) sys.error(
        s"Type mismatch, expected $typeId (0x${typeId.toHexString}) but found $tpe (0x${tpe.toHexString})")
      readIdentifiedObj(in, access)
    }

    override def readIdentifiedObj[S <: Base[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S]
  }

  def addType(tpe: Type): Unit      = Impl.addType(tpe)
  def getType(id : Int ): Obj.Type  = Impl.getType(id )

  // ---- attributes ----

  type AttrMap    [S <: Base[S]]            = evt.Map.Modifiable[S, String, Obj]
  type AttrUpdate [S <: Sys[S]]             = evt.Map.Update [S, String, Obj]
  val  AttrAdded    : evt.Map.Added.type    = evt.Map.Added
  type AttrAdded  [S <: Sys[S]]             = evt.Map.Added  [S, String, Obj[S]]
  val  AttrRemoved  : evt.Map.Removed.type  = evt.Map.Removed
  type AttrRemoved[S <: Sys[S]]             = evt.Map.Removed[S, String, Obj[S]]
  val  AttrReplaced : evt.Map.Replaced.type = evt.Map.Replaced
  type AttrReplaced[S <: Sys[S]]            = evt.Map.Replaced[S, String, Obj[S]]

  /* implicit */ def attrMapSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, AttrMap[S]] =
    anyAttrMapSer.asInstanceOf[Serializer[S#Tx, S#Acc, AttrMap[S]]]

  private[this] val anyAttrMapSer = evt.Map.Modifiable.serializer[NoSys, String, Obj]

  final val attrName = "name"
}

/** An `Obj` is a type of element that has an `S#Id` identifier and
  * an attribute map. It can be the origin of event dispatch.
  */
trait Obj[S <: Base[S]] extends Elem[S] with stm.Mutable[S#Id, S#Tx] {
  override def toString = s"Obj$id"

  override def tpe: Obj.Type

  def attr(implicit tx: S#Tx): Obj.AttrMap[S]

//  final def attr(implicit tx: S#Tx): Obj.AttrMap[S] = tx.attrMap(this)
}