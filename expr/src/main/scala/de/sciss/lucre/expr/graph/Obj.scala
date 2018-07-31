/*
 *  Obj.scala
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

import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.expr.graph.Attr.Bridge
import de.sciss.lucre.expr.graph.impl.ExpandedAttrUpdate
import de.sciss.lucre.stm.{Disposable, Sys}

object Obj {
  object Attr {
    final case class Update[A](source: Ex[A], key: String, child: String)(implicit bridge: Bridge[A])
      extends Control with ProductWithAux {

      override def productPrefix: String = s"Obj$$Attr$$Update"

      type Repr[S <: Sys[S]] = Disposable[S#Tx]

      @inline
      private def dummyRes[S <: Sys[S]] =
        Disposable.empty[S#Tx]

      protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S] =
        ctx.selfOption.fold(dummyRes[S]) { self =>
          self.attr.get(key).fold(dummyRes[S]) { childObj =>
            val attrView = bridge.cellView[S](childObj, child)
            new ExpandedAttrUpdate[S, A](source.expand[S], attrView, tx)
          }
        }

      override def aux: scala.List[Aux] = bridge :: Nil
    }

    final case class WithDefault[A](key: String, child: String, default: Ex[A])(implicit bridge: Bridge[A])
      extends graph.Attr.WithDefault[A] {

      override def productPrefix = s"Obj$$Attr$$WithDefault"

      def update(in: Ex[A]): Control = Obj.Attr.Update(in, key = key, child = child)

      def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, A] = {
        val defaultEx = default.expand[S]
        ctx.selfOption.fold(defaultEx) { self =>
          self.attr.get(key).fold(defaultEx) { childObj =>
            import ctx.targets
            val attrView = bridge.cellView[S](childObj, child)
            new graph.Attr.WithDefault.Expanded[S, A](attrView, defaultEx, tx)
          }
        }
      }

      override def aux: List[Aux] = bridge :: Nil
    }
  }
  final case class Attr[A](key: String, child: String)(implicit val bridge: Bridge[A])
    extends Ex[Option[A]] with graph.Attr.Like[A] {

    override def productPrefix = s"Obj$$Attr"

    @inline
    private def dummyRes[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx)=
      Constant(Option.empty[A]).expand[S]

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, Option[A]] =
      ctx.selfOption.fold(dummyRes) { self =>
        self.attr.get(key).fold(dummyRes) { childObj =>
         import ctx.targets
          val attrView = bridge.cellView[S](childObj, child)
          new graph.Attr.Expanded[S, A](attrView, tx)
        }
      }

    def update(in: Ex[A]): Control = Obj.Attr.Update(in, key = key, child = child)

    override def aux: List[Aux] = bridge :: Nil
  }
}
final case class Obj(key: String) extends Product {
  def attr[A: Bridge](child: String): Obj.Attr[A] =
    Obj.Attr(key = key, child = child)

  def attr[A: Bridge](child: String, default: Ex[A]): Obj.Attr.WithDefault[A] =
    Obj.Attr.WithDefault(key = key, child = child, default = default)
}
