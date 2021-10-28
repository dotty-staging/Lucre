/*
 *  ITrigger.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.{Disposable, Exec, Form, IPublisher}

trait ITrigger[T <: Exec[T]] extends Form[T] with IPublisher[T, Unit] with Disposable[T]
