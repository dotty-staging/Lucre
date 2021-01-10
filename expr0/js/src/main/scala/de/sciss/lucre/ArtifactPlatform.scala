/*
 *  ArtifactPlatform.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import java.net.URI

trait ArtifactPlatform {
  private[sciss] def fileToURI(path: String): URI =
    throw new UnsupportedOperationException
}
