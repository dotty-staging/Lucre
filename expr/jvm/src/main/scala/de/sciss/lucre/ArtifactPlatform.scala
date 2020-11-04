/*
 *  ArtifactPlatform.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import java.io.File

import de.sciss.lucre.Artifact.{Child, Value}

import scala.annotation.tailrec

trait ArtifactPlatform {
  private def requireFile(u: Value): Unit = {
    val s = u.getScheme
    if (s != "file")
      throw new IllegalStateException(s"Not a supported artifact URI scheme: $s")
  }

  def relativize(parent: Value, sub: Value): Child = {
    requireFile(parent)
    requireFile(sub   )

    // Note: .getCanonicalFile will resolve symbolic links.
    // In order to support artifacts being symbolic links
    // inside a parent folder, we must not resolve them!

    val parentF = new File(parent )
    val subF    = new File(sub    )
    val can     = subF   .getAbsoluteFile // .getCanonicalFile
    val base    = parentF.getAbsoluteFile // .getCanonicalFile

    @tailrec def loop(res: File, left: File): File = {
      if (left == null)
        throw new IllegalArgumentException(s"File $sub is not in a subdirectory of $parent")

      if (left == base) res
      else {
        val last  = left.getName
        val init  = left.getParentFile
        loop(new File(last, res.getPath), init)
      }
    }

    val cf = loop(new File(can.getName), can.getParentFile)
    Child(cf.getPath)
  }
}
