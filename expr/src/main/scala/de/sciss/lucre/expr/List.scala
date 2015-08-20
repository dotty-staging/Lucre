/*
 *  List.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.impl.{ListImpl => Impl}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{data, event => evt}
import de.sciss.serial.{DataInput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object List extends Obj.Type {
  final val typeID  = 23

  //  val Type: Type3[List] = Impl.TypeImpl
  //
  //  implicit def Ops[S <: Sys[S], Elem, U](list: List[S, Elem, U]): Ops[S, Elem] = new Impl.Ops(list)
  //
  //  trait Ops[S <: Sys[S], Elem] extends Any {
  //    def isEmpty_@   (implicit tx: S#Tx): Expr[S, Boolean]
  //    def nonEmpty_@  (implicit tx: S#Tx): Expr[S, Boolean]
  //    def size_@      (implicit tx: S#Tx): Expr[S, Int    ]
  //  }

  final case class Update[S <: Sys[S], Elem](list: List[S, Elem], changes: Vec[Change[S, Elem]])

  sealed trait Change[S <: Sys[S], Elem] {
    def index: Int
    def elem: Elem
  }

  final case class Added[S <: Sys[S], Elem](index: Int, elem: Elem)
    extends Change[S, Elem]

  final case class Removed[S <: Sys[S], Elem](index: Int, elem: Elem)
    extends Change[S, Elem]

  object Modifiable {
    /** Returns a serializer for a modifiable list. */
    implicit def serializer[S <: Sys[S], Elem <: Obj[S]]: Serializer[S#Tx, S#Acc, Modifiable[S, Elem]] =
      Impl.modSerializer[S, Elem]

    def read[S <: Sys[S], Elem <: Obj[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S, Elem] =
      serializer[S, Elem].read(in, access)

    /** Creates a new empty linked list. */
    def apply[S <: Sys[S], Elem <: Obj[S]](implicit tx: S#Tx): Modifiable[S, Elem] =
      Impl.newModifiable[S, Elem]
  }

  /** Modifiable extension of the linked list. Elements can be appended or prepended in O(1).
    * Removal of the head or last element is O(1). Arbitrary removal takes O(N).
    */
  trait Modifiable[S <: Sys[S], Elem] extends List[S, Elem] with evt.Node[S] {
    def addLast(elem: Elem)(implicit tx: S#Tx): Unit
    def addHead(elem: Elem)(implicit tx: S#Tx): Unit

    def removeLast()(implicit tx: S#Tx): Elem
    def removeHead()(implicit tx: S#Tx): Elem

    def insert  (index: Int, elem: Elem)(implicit tx: S#Tx): Unit
    def remove  (elem: Elem)(implicit tx: S#Tx): Boolean
    def removeAt(index: Int)(implicit tx: S#Tx): Elem

    def clear()(implicit tx: S#Tx): Unit
  }

  implicit def serializer[S <: Sys[S], Elem <: Obj[S]]: Serializer[S#Tx, S#Acc, List[S, Elem]] =
    Impl.serializer[S, Elem]

  def read[S <: Sys[S], Elem <: Obj[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): List[S, Elem] =
    serializer[S, Elem].read(in, access)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}

/** An observable linked list with fast `head` and `last` operations.
  * This is the read-only layer, see `List.Modifiable` for a mutable list.
  *
  * The list will report insertions and deletions.
  *
  * @tparam Elem      the element type of the list
  */
trait List[S <: Sys[S], Elem] extends Obj[S] with Publisher[S, List.Update[S, Elem]] {
  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean
  def size    (implicit tx: S#Tx): Int

  def apply(index: Int)(implicit tx: S#Tx): Elem
  def get  (index: Int)(implicit tx: S#Tx): Option[Elem]

  def headOption(implicit tx: S#Tx): Option[Elem]
  def lastOption(implicit tx: S#Tx): Option[Elem]

  def head(implicit tx: S#Tx): Elem
  def last(implicit tx: S#Tx): Elem

  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Elem]

  def modifiableOption: Option[List.Modifiable[S, Elem]]

  /** Note: this is an O(n) operation. */
  def indexOf(elem: Elem)(implicit tx: S#Tx): Int
}