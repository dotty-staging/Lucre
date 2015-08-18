package de.sciss

import de.sciss.serial.Writable

package object lucre {
  type Mutable[+ID, -Tx] = Identifiable[ID] with Writable with Disposable[Tx]
}
