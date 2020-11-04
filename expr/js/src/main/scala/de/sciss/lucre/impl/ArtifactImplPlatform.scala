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

import java.net.URI

import de.sciss.lucre.Artifact.Value

trait ArtifactImplPlatform {
  protected final def concat(parent: URI, child: String): Value = {
    val parent0 = parent.getPath
    val parentS = if (parent0.isEmpty || parent0.endsWith("/")) parent0 else s"$parent0/"
    val path    = s"$parentS$child"
    new URI("idb", path, null)
  }

  private[lucre] def listFiles[T <: Txn[T]](dv: URI)(implicit tx: T): Seq[URI] = ???
}
