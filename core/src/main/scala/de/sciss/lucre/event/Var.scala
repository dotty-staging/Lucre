//package de.sciss.lucre.event
//
//import de.sciss.lucre.stm.{Disposable, Sink, Sys}
//import de.sciss.serial.Writable
//
//trait Var[S <: Sys[S], A] extends Sink[S#Tx, A] with Writable with Disposable[S#Tx] {
//  def get                     (implicit tx: S#Tx): Option[A]
//  def getOrElse(default: => A)(implicit tx: S#Tx): A
//  def transform(default: => A)(f: A => A)(implicit tx: S#Tx): Unit
//}