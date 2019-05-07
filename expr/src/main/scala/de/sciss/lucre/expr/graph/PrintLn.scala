/*
 *  PrintLn.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.stm.Sys

object PrintLn {
  private final class Expanded[S <: Sys[S]](text: IExpr[S, String]) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit = {
      val s = text.value
      tx.afterCommit {
        println(s)
      }
    }
  }
}
final case class PrintLn(text: Ex[String]) extends Act {
  def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IAction[S] =
    new PrintLn.Expanded(text.expand[S])
}
