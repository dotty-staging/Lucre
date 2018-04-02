/*
 *  Var.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import de.sciss.serial.Writable

object Sink {
  def map[T, A, B](in: Sink[T, A])(fun: B => A): Sink[T, B] = new Map(in, fun)

  private final class Map[T, A, B](in: Sink[T, A], fun: B => A)
    extends Sink[T, B] {

    override def toString = s"Sink.map($in)"

    def update(v: B)(implicit tx: T): Unit = in() = fun(v)
  }
}

/** A sink is a transactional write access to a value */
trait Sink[-T, -A] {
  def update(v: A)(implicit tx: T): Unit
}

object Source {
  def map[T, A, B](in: Source[T, A])(fun: A => B): Source[T, B] = new Map(in, fun)

  private final class Map[T, A, B](in: Source[T, A], fun: A => B)
    extends Source[T, B] {

    override def toString = s"Source.map($in)"

    def apply()(implicit tx: T): B = fun(in())
  }
}

/** A source is a transactional read access to a value */
trait Source[-T, +A] {
  def apply()(implicit tx: T): A
}

//trait LocalVar[-Tx, A] extends Sink[Tx, A] with Source[Tx, A] {
//  def isInitialized(implicit tx: Tx): Boolean
//}

/** A transactional variable is an identifiable cell allowing the reading and writing of values */
trait VarLike[-T, A] extends Sink[T, A] with Source[T, A]

trait Var[-T, A] extends VarLike[T, A] with Writable with Disposable[T]