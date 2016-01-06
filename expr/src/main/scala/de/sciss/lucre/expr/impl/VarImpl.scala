/*
 *  VarImpl.scala
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

package de.sciss.lucre.expr
package impl

import de.sciss.lucre.event.{Pull, impl => evti}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change
import de.sciss.serial.DataOutput

trait VarImpl[S <: Sys[S], A, Repr <: Expr[S, A]]
  extends Expr[S, A] with stm.Var[S#Tx, Repr] // .Var[S, A]
  with NodeImpl[S, A] { self =>

  // ---- abstract ----

  protected def ref: S#Var[Repr]

  // ---- implemented ----

  object changed extends Changed with evti.Generator[S, Change[A]] {
    private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Change[A]] =
      if (pull.parents(this).isEmpty) {
        Some(pull.resolve[Change[A]])
      } else {
        pull(self().changed)
      }
  }

  final protected def writeData(out: DataOutput): Unit = {
    out.writeByte(0)
    ref.write(out)
  }

  final protected def disposeData()(implicit tx: S#Tx): Unit = {
    disconnect()
    ref.dispose()
  }

  final def connect()(implicit tx: S#Tx): this.type = {
    ref().changed ---> changed
    this
  }

  private[this] def disconnect()(implicit tx: S#Tx): Unit = ref().changed -/-> changed

  final def apply()(implicit tx: S#Tx): Repr = ref()

  final def update(expr: Repr)(implicit tx: S#Tx): Unit = {
    val before = ref()
    if (before != expr) {
      before.changed -/-> this.changed
      ref() = expr
      expr  .changed ---> this.changed

      val beforeV = before.value
      val exprV   = expr  .value
      changed.fire(Change(beforeV, exprV))
    }
  }

  final def value(implicit tx: S#Tx): A = ref().value

  override def toString = s"Expr.Var$id"
}