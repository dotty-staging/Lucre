///*
// *  Context.scala
// *  (Lucre)
// *
// *  Copyright (c) 2009-2015 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Lesser General Public License v2.1+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre.stm
//
//import de.sciss.serial.Writable
//
//trait Context[S <: Sys[S]] extends Writable with Disposable[S#Tx] {
////  def get  [A](vr: S#Var[A])(implicit tx: S#Tx): Option[A]
////  def apply[A](vr: S#Var[A])(implicit tx: S#Tx): A
//}
//
//trait LongContext[S <: Sys[S]] extends Context[S] {
//  def position: Sink[S#Tx, Long] with Source[S#Tx, Long]
//}