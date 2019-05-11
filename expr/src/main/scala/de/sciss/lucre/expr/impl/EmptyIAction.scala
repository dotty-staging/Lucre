package de.sciss.lucre.expr.impl

import de.sciss.lucre.expr.{IAction, ITrigger}
import de.sciss.lucre.stm.Base

final class EmptyIAction[S <: Base[S]] extends IAction[S] {
  def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit = ()

  def executeAction()(implicit tx: S#Tx): Unit = ()

  def dispose()(implicit tx: S#Tx): Unit = ()
}
