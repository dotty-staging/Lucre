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
import de.sciss.lucre.stm.{Elem, Obj, Sys}
import de.sciss.lucre.{data, event => evt}
import de.sciss.serial.{DataInput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds
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

  final case class Update[S <: Sys[S], A](list: List[S, A], changes: Vec[Change[S, A]])

  sealed trait Change[S <: Sys[S], A] {
    def index: Int
    def elem: A
  }

  final case class Added[S <: Sys[S], A](index: Int, elem: A)
    extends Change[S, A]

  final case class Removed[S <: Sys[S], A](index: Int, elem: A)
    extends Change[S, A]

  object Modifiable {
    /** Returns a serializer for a modifiable list. */
    implicit def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, Modifiable[S, A]] =
      Impl.modSerializer[S, A]

    def read[S <: Sys[S], A <: Elem[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S, A] =
      serializer[S, A].read(in, access)

    /** Creates a new empty linked list. */
    def apply[S <: Sys[S], E[~ <: Sys[~]] <: Elem[~]](implicit tx: S#Tx): Modifiable[S, E[S]] =
      Impl.newModifiable[S, E]
  }

  /** Modifiable extension of the linked list. Elements can be appended or prepended in O(1).
    * Removal of the head or last element is O(1). Arbitrary removal takes O(N).
    */
  trait Modifiable[S <: Sys[S], A] extends List[S, A] with evt.Node[S] {
    def addLast(elem: A)(implicit tx: S#Tx): Unit
    def addHead(elem: A)(implicit tx: S#Tx): Unit

    def removeLast()(implicit tx: S#Tx): A
    def removeHead()(implicit tx: S#Tx): A

    def insert  (index: Int, elem: A)(implicit tx: S#Tx): Unit
    def remove  (elem: A)(implicit tx: S#Tx): Boolean
    def removeAt(index: Int)(implicit tx: S#Tx): A

    def clear()(implicit tx: S#Tx): Unit
  }

  implicit def serializer[S <: Sys[S], A <: Elem[S]]: Serializer[S#Tx, S#Acc, List[S, A]] =
    Impl.serializer[S, A]

  def read[S <: Sys[S], A <: Elem[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): List[S, A] =
    serializer[S, A].read(in, access)

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

//  /** Hint key for copying the list. The value is a predicate function `(A) => Boolean`
//    * to determine which elements to copy. If absent, the entire list is copied
//    */
//  final val hintFilter = "filter"
}

/** An observable linked list with fast `head` and `last` operations.
  * This is the read-only layer, see `List.Modifiable` for a mutable list.
  *
  * The list will report insertions and deletions.
  *
  * @tparam A      the element type of the list
  */
trait List[S <: Sys[S], A] extends Obj[S] with Publisher[S, List.Update[S, A]] {
  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean
  def size    (implicit tx: S#Tx): Int

  def apply(index: Int)(implicit tx: S#Tx): A
  def get  (index: Int)(implicit tx: S#Tx): Option[A]

  def headOption(implicit tx: S#Tx): Option[A]
  def lastOption(implicit tx: S#Tx): Option[A]

  def head(implicit tx: S#Tx): A
  def last(implicit tx: S#Tx): A

  def iterator(implicit tx: S#Tx): Iterator[A]

  def modifiableOption: Option[List.Modifiable[S, A]]

  /** Note: this is an O(n) operation. */
  def indexOf(elem: A)(implicit tx: S#Tx): Int
}