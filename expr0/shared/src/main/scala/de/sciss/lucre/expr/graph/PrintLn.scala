/*
 *  PrintLn.scala
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

package de.sciss.lucre.expr
package graph

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.{IExpr, Txn}

object PrintLn extends ProductReader[PrintLn] {
  private final class Expanded[T <: Txn[T]](text: IExpr[T, String]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit = {
      val s = text.value
      tx.afterCommit {
        println(s)
      }
    }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): PrintLn = {
    require (arity == 1 && adj == 0)
    val _text = in.readEx[String]()
    new PrintLn(_text)
  }
}
final case class PrintLn(text: Ex[String]) extends Act {
  type Repr[T <: Txn[T]] = IAction[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new PrintLn.Expanded(text.expand[T])
}
