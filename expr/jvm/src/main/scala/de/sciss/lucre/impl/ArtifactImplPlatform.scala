/*
 *  ArtifactImplPlatform.scala
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
package impl

import java.io.File
import java.net.URI

import de.sciss.lucre.Artifact.Value

trait ArtifactImplPlatform {
  protected final def concat(parent: URI, child: String): Value = {
    val parentF = new File(parent)
    val f       = new File(parentF, child)
    f.toURI
  }

  private[lucre] def listFiles[T <: Txn[T]](dv: URI)(implicit tx: T): Seq[URI] = {
    val dvf     = new File(dv)
    val arr     = dvf.listFiles()
    val now     = if (arr == null) Nil else arr.toSeq
    now.map(_.toURI)
  }
}
