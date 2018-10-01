/*
 *  Workspace.scala
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

import java.io.File

import de.sciss.lucre.stm

object Workspace {
  object Implicits {
    implicit def dummy[S <: Sys[S]](implicit system: S, cursor: stm.Cursor[S]): Workspace[S] =
      new DummyImpl[S](system, cursor)
    // dummyVal.asInstanceOf[DummyImpl[S]]

//    private val dummyVal = new DummyImpl[NoSys]

    private final case class DummyImpl[S <: Sys[S]](system: S, cursor: stm.Cursor[S])
      extends Workspace[S] {

      def addDependent   (dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit = ()
      def removeDependent(dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit = ()

      def dependents(implicit tx: TxnLike): Iterable[Disposable[S#Tx]] = Nil

      def folder: Option[File] = None

      def name: String = "dummy"

//      def close(): Unit = ()

      def dispose()(implicit tx: S#Tx): Unit = ()

      def root(implicit tx: S#Tx): Folder[S] =
        throw new UnsupportedOperationException("No root folder on a dummy workspace handle")
    }
  }
}
trait Workspace[S <: Sys[S]] extends Disposable[S#Tx] {
  implicit def system: S

  def cursor: stm.Cursor[S]

  def folder: Option[File]

  def name: String

//  /** Issues a transaction that closes and disposes the workspace.
//    */
//  def close(): Unit

  /** Adds a dependent which is disposed just before the workspace is disposed.
    *
    * @param dep  the dependent. This must be an _ephemeral_ object.
    */
  def addDependent   (dep: Disposable[S#Tx])(implicit tx: TxnLike /* S#Tx */): Unit
  def removeDependent(dep: Disposable[S#Tx])(implicit tx: TxnLike /* S#Tx */): Unit

  def dependents(implicit tx: TxnLike): Iterable[Disposable[S#Tx]]

  def root(implicit tx: S#Tx): Folder[S]
}
