/*
 *  IActionImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.impl

import de.sciss.lucre.expr.{IAction, ITrigger}
import de.sciss.lucre.stm.TxnLike.{peer => txPeer}
import de.sciss.lucre.stm.{Disposable, Sys}

import scala.concurrent.stm.Ref

trait IActionImpl[S <: Sys[S]] extends IAction[S] {
  private[this] val disposables = Ref(List.empty[Disposable[S#Tx]])

  protected final def addDisposable(d: Disposable[S#Tx])(implicit tx: S#Tx): Unit =
    disposables.transform(d :: _)

  def dispose()(implicit tx: S#Tx): Unit =
    disposables.swap(Nil).foreach(_.dispose())

  def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit = {
    val obs = tr.changed.react { implicit tx => _ => executeAction() }
    addDisposable(obs)
  }
}
