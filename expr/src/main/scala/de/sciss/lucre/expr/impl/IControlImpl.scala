/*
 *  IControlImpl.scala
 *  (LucreSwing)
 *
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.expr.impl

import de.sciss.lucre.expr.{Control, Ex, IControl}
import de.sciss.lucre.stm.{Disposable, Sys}

trait IControlImpl[S <: Sys[S]] extends IControl[S] {
  private[this] var obs = List.empty[Disposable[S#Tx]]

  protected def peer: Control

  /** The default does nothing. Sub-classes may override this. */
  def initControl()(implicit tx: S#Tx): Unit = ()

  protected final def initProperty[A](key: String, default: A)(set: S#Tx => A => Unit)
                                     (implicit tx: S#Tx, ctx: Ex.Context[S]): Unit =
    ctx.getProperty[Ex[A]](peer, key) match {
      case Some(ex) =>
        val expr    = ex.expand[S]
        val value0  = expr.value
        if (value0 != default) {
          set(tx)(value0)
        }
        obs ::= expr.changed.react { implicit tx => upd =>
          set(tx)(upd.now)
        }

      case _ =>
    }

  def dispose()(implicit tx: S#Tx): Unit =
    obs.foreach(_.dispose())
}
