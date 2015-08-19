/*
 *  BasicTxnImpl.scala
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

package de.sciss.lucre.stm.impl

import de.sciss.lucre.stm.{Sys, Txn}

import scala.concurrent.stm.Txn

trait BasicTxnImpl[S <: Sys[S]] extends Txn[S] {
  _: S#Tx =>

  def beforeCommit(fun: S#Tx => Unit): Unit =
    Txn.beforeCommit(_ => fun(this))(peer)

  def afterCommit(code: => Unit): Unit =
    Txn.afterCommit(_ => code)(peer)
}