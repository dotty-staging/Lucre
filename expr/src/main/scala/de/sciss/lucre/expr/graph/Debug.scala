/*
 *  Debug.scala
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

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.stm.Sys

object Debug {
  object PrintNow {
    private final class Expanded[S <: Sys[S]](text: IExpr[S, String]) extends IActionImpl[S] {
      def executeAction()(implicit tx: S#Tx): Unit = {
        val s = text.value
        print(s)
      }
    }
  }
  /** An action that prints a text expression "now" that is directly inside a transaction,
    * not waiting for the commit of the transaction. This is for debugging purposes, when
    * chasing exceptions which would otherwise swallow regular `PrintLn` actions.
    * Note that this action does not append a new-line, so one has to include a `\n` in
    * the string if needed.
    */
  final case class PrintNow(text: Ex[String]) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"Debug$$PrintNow"  // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new PrintNow.Expanded(text.expand[S])
  }
}
