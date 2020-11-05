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

import de.sciss.lucre.Artifact.Value

trait ArtifactImplPlatform {
  private[lucre] def listFiles[T <: Txn[T]](dv: Value)(implicit tx: T): Seq[Value] = ???
}