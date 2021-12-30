/*
 *  TestUtil.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

object TestUtil {
  def formatSeconds(seconds: Double): String = {
    val millisR = (seconds * 1000).toInt
    val sb      = new StringBuilder(10)
    val secsR   = millisR / 1000
    val millis  = millisR % 1000
    val mins    = secsR / 60
    val secs    = secsR % 60
    if (mins > 0) {
      sb.append(mins)
      sb.append(':')
      if (secs < 10) {
        sb.append('0')
      }
    }
    sb.append(secs)
    sb.append('.')
    if (millis < 10) {
      sb.append('0')
    }
    if (millis < 100) {
      sb.append('0')
    }
    sb.append(millis)
    sb.append('s')
    sb.toString()
  }

  /** You can run sbt with `-Dde.sciss.lucre.ShortTests=true` to bring
    * down test time, e.g. when running on CI community build.
    */
  lazy val isShort: Boolean = {
    val b = sys.props.get("de.sciss.lucre.ShortTests").exists(_.toLowerCase == "true")
    if (b) println("Lucre: using shorter test cycle")
    b
  }
}