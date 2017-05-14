/*
 *  BasicTxnImpl.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm
package impl

import scala.annotation.meta.field
import scala.concurrent.stm.{Txn => ScalaTxn}

trait BasicTxnImpl[S <: Sys[S]] extends Txn[S] {
  _: S#Tx =>

  def beforeCommit(fun: S#Tx => Unit): Unit =
    ScalaTxn.beforeCommit(_ => fun(this))(peer)

  def afterCommit(code: => Unit): Unit =
    ScalaTxn.afterCommit(_ => code)(peer)

  @field protected var _context: S#Context = _

  final def use[A](context: S#Context)(fun: => A): A = {
    val old   = _context
    _context  = context
    try {
      fun
    } finally {
      _context = old
    }
  }
}