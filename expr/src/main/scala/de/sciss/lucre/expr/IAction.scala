/*
 *  IAction.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.stm.{Base, Disposable, Form}

trait IAction[S <: Base[S]] extends Form[S] with Disposable[S#Tx] {
  /** Directly adds a trigger input to the action.
    * Actions that do not produce successive events can
    * simply rewrite this as
    *
    * {{{
    * tr.changed.react { implicit tx => _ => executeAction() }
    * }}}
    *
    * If the action produces successive events, it should
    * prevent this indirection, as triggered cannot be logically
    * combined that way.
    */
  def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit

  def executeAction()(implicit tx: S#Tx): Unit
}
