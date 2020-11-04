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

import de.sciss.lucre.Artifact.{Child, Value}

trait ArtifactPlatform {
  private def requireFile(u: Value): Unit = {
    val s = u.getScheme
    if (s != "idb")
      throw new IllegalStateException(s"Not a supported artifact URI scheme: $s")
  }

  def relativize(parent: Value, sub: Value): Child = {
    requireFile(parent)
    requireFile(sub   )

    val parentP = parent.getPath
    val subP    = sub   .getPath

    if (!subP.startsWith(parentP))
      throw new IllegalArgumentException(s"File $subP is not in a subdirectory of $parentP")

    val res0    = subP.substring(parentP.length)
    val res     = if (res0.isEmpty || res0.charAt(0) != '/') res0 else res0.substring(1)
    Child(res)
  }
}
