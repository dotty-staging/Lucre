package de.sciss.lucre

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable

package object stm {
  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'stm' ", Locale.US)
  var showLog = false

  @elidable(elidable.CONFIG) private[lucre] def log(what: => String): Unit =
    if (showLog) println(logHeader.format(new Date()) + what)
}