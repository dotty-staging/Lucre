//package de.sciss.lucre.event
//
//import de.sciss.lucre.stm.Disposable
//import de.sciss.serial.Writable
//
//trait Validity[-Tx] extends Writable with Disposable[Tx] {
//  def apply ()(implicit tx: Tx): Boolean
//  def update()(implicit tx: Tx): Unit
//}