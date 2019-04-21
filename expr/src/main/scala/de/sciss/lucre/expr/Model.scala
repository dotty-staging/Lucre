/*
 *  Model.scala
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

import de.sciss.lucre.expr.graph.Attr

trait Model[A] {
  def apply(): Ex[A]
  def update(value: Ex[A]): Unit

  def <--> (attr: Attr.WithDefault[A]): Unit = {
    this <--- attr
    this ---> attr
  }

  def ---> (attr: Attr.Like[A]): Unit = {
    import ExOps._
    apply() ---> attr
  }

  def ---> (m: Model[A]): Unit =
    m <--- this

  def <--- (value: Ex[A]): Unit =
    update(value)

  def <--- (m: Model[A]): Unit =
    update(m())
}
