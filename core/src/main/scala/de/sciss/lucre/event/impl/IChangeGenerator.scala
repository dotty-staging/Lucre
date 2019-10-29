package de.sciss.lucre.event.impl

import de.sciss.lucre.stm.Base
import de.sciss.model.Change

trait IChangeGenerator[S <: Base[S], A] extends IGenerator[S, Change[A]] with IChangeEventImpl[S, A]