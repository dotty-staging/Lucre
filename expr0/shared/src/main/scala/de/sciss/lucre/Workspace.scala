/*
 *  Workspace.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import java.net.URI

object Workspace {
  object Implicits {
    implicit def dummy[T <: Txn[T]](implicit system: Sys, cursor: Cursor[T]): Workspace[T] =
      new DummyImpl[T](system, cursor)
    // dummyVal.asInstanceOf[DummyImpl[T]]

    //    private val dummyVal = new DummyImpl[NoSys]

    private final class DummyImpl[T <: Txn[T]](val system: Sys, val cursor: Cursor[T])
      extends Workspace[T] {

      type S = Sys

      def addDependent    (dep: Disposable[T])(implicit tx: TxnLike): Unit = ()
      def removeDependent (dep: Disposable[T])(implicit tx: TxnLike): Unit = ()

      def dependents(implicit tx: TxnLike): Iterable[Disposable[T]] = Nil

      def folder: Option[URI] = None

      def name: String = "dummy"

      def close(): Unit = ()

      def dispose()(implicit tx: T): Unit = ()

      override def toString: String = s"Workspace.Implicits.dummy($system)@${hashCode().toHexString}"

      def root(implicit tx: T): Folder[T] =
        throw new UnsupportedOperationException("No root folder on a dummy workspace handle")
    }
  }
}
trait Workspace[T <: Txn[T]] extends Disposable[T] {
  type S <: Sys
  type Tx = T

  implicit def system: S

  /** Since we obtain `Workspace[_]` from read methods, this is lesser evil, since we
   * cannot make totally "wrong" casts here. */
  def cast[T1 <: Txn[T1]]: Workspace[T1] = this.asInstanceOf[Workspace[T1]]

  def cursor: Cursor[T]

  def folder: Option[URI]

  def name: String

  /** Issues a transaction that closes and disposes the workspace. */
  def close(): Unit

  /** Adds a dependent which is disposed just before the workspace is disposed.
   *
   * @param dep  the dependent. This must be an _ephemeral_ object.
   */
  def addDependent(dep: Disposable[T])(implicit tx: TxnLike /* T */): Unit
  def removeDependent(dep: Disposable[T])(implicit tx: TxnLike /* T */): Unit

  def dependents(implicit tx: TxnLike): Iterable[Disposable[T]]

  def root(implicit tx: T): Folder[T]
}
