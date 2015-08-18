/*
 *  package.scala
 *  (Lucre)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.serial

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.collection.immutable.{IndexedSeq => Vec}

package object event {
  type Reaction = () => () => Unit

  private[event] type Children[S <: stm.Sys[S]] = Vec[(Byte, Selector[S])]

  private val emptySeq = Vec.empty[Nothing]

  private[lucre] def NoChildren[S <: stm.Sys[S]]: Children[S] = emptySeq

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'evt' ", Locale.US)

  var showLog = false

  @elidable(CONFIG) private[event] def log(what: => String): Unit =
    if (showLog) println(logHeader.format(new Date()) + what)
}