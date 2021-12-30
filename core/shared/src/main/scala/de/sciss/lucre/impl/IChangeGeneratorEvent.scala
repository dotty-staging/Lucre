/*
 *  IChangeGeneratorEvent.scala
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
package impl

import de.sciss.model.Change

trait IChangeGeneratorEvent[T <: Exec[T], A] extends IGeneratorEvent[T, Change[A]] with IChangeEventImpl[T, A]