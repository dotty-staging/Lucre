/*
 *  Println.scala
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

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.stm.Sys

object Println {
  private final class Expanded[S <: Sys[S]](text: IExpr[S, String]) extends IAction[S] {
    def execute()(implicit tx: S#Tx): Unit = {
      val s = text.value
      tx.afterCommit {
        println(s)
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
final case class Println(text: Ex[String]) extends Act {
  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IAction[S] =
    new Println.Expanded(text.expand[S])
}
