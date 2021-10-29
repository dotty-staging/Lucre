/*
 *  OptionGet.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.{IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change

object OptionGet extends ProductReader[OptionGet[_]] {
  private final class Expanded[T <: Txn[T], A](in: IExpr[T, Option[A]], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, Option[A], A](in, tx0) {

    override def toString: String = s"OptionGet($in)"

    protected def mapValue(inValue: Option[A])(implicit tx: T): A = inValue.get

    override private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Change[A]] =
      pull(in.changed).flatMap { ch =>
        // simply don't fire if the option was not or is not defined
        if (ch.before.isEmpty || ch.now.isEmpty) None else {
          val before  = mapValue(ch.before )
          val now     = mapValue(ch.now    )
          if (before == now) None else Some(Change(before, now))
        }
      }
  }

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OptionGet[_] = {
    require (arity == 1 && adj == 0)
    val _in = in.readEx[Option[Any]]()
    new OptionGet(_in)
  }
}
@deprecated("Use UnaryOp.OptionGet instead", since = "4.5.1")
case class OptionGet[A](in: Ex[Option[A]]) extends Ex[A] {
  type Repr[T <: Txn[T]] = IExpr[T, A]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
    import ctx.targets
    new OptionGet.Expanded(in.expand[T], tx)
  }
}
