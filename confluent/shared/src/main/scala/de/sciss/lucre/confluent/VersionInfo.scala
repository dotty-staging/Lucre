/*
 *  VersionInfo.scala
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

package de.sciss.lucre.confluent

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

object VersionInfo {

  trait Modifiable extends VersionInfo {
    var message: String
  }

  def apply(message: String, timeStamp: Long): VersionInfo = Impl(message, timeStamp)

  private val df = new SimpleDateFormat("d MMM yyyy, HH:mm''ss.SSS", Locale.US)

  private final case class Impl(message: String, timeStamp: Long) extends VersionInfo {
    override def toString: String = {
      val m = if (message != "") s"$message, " else ""
      s"VersionInfo(${m}date = ${df.format(new Date(timeStamp))})"
    }
  }

}
trait VersionInfo {
  /** (Possibly empty) message describing the associated transaction. This can be seen as similar to
    * a commit message, the difference being that it must be given (currently) at the moment the
    * transaction is opened and not at the moment when it is closed.
    */
  def message: String

  /** System time stamp of the associated transaction. The format and offset corresponds to
    * `System.currentTimeMillis`.
    */
  def timeStamp: Long
}