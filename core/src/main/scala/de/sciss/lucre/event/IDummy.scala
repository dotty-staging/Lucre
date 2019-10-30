/*
 *  IDummy.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event

import de.sciss.lucre.stm.{Base, Disposable, NoBase}
import de.sciss.model.Change

object IDummy {
  /** This method is cheap. */
  def apply[S <: Base[S], A]: IEvent[S, A] = anyDummy.asInstanceOf[IEvent[S, A]]

  def applyChange[S <: Base[S], A]: IChangeEvent[S, A] = anyChangeDummy.asInstanceOf[IChangeEvent[S, A]]

  private val anyDummy        = new Impl       [NoBase]
  private val anyChangeDummy  = new ChangeImpl [NoBase]

  private final class Impl[S <: Base[S]] extends IEvent[S, Any] {
    override def toString = "event.IDummy"

    def --->(sink: IEvent[S, Any])(implicit tx: S#Tx): Unit = ()
    def -/->(sink: IEvent[S, Any])(implicit tx: S#Tx): Unit = ()

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Any] = None

    def react(fun: S#Tx => Any => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = Observer.dummy[S]
  }

  private final class ChangeImpl[S <: Base[S]] extends IChangeEvent[S, Any] {
    override def toString = "event.IDummy"

    def --->(sink: IEvent[S, Any])(implicit tx: S#Tx): Unit = ()
    def -/->(sink: IEvent[S, Any])(implicit tx: S#Tx): Unit = ()

    private[lucre] def pullChange(pull: IPull[S], isNow: Boolean)(implicit tx: S#Tx): Any = ()

    def react(fun: S#Tx => Change[Any] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = Observer.dummy[S]
  }
}