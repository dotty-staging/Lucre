//package de.sciss.sandbox
//
//import de.sciss.lucre.stm.Tx
//import de.sciss.serial.{DataInput, DataOutput}
//
//import scala.language.higherKinds
//
//object SerTest {
//  trait Serializer[A[_ <: Tx]] {
//    type A1[T <: Tx] = A[T]
//
//    def write[T <: Tx](value: A1[T], out: DataOutput): Unit
//    def read [T <: Tx](in: DataInput, tx: T)(implicit access: tx.Acc): A1[T]
//  }
//
//  trait Foo[T <: Tx]
//
//  trait FooSer extends Serializer[Foo] {
//    def write[T <: Tx](foo: Foo[T], out: DataOutput): Unit
//    def read [T <: Tx](in: DataInput, tx: T)(implicit access: tx.Acc): Foo[T]
//  }
//
//  object ImmutableSerializer {
//    type NonTxn[A]
//  }
//  trait ImmutableSerializer[B] extends Serializer[({type A[_ <: Tx] = B})#A] {
//    def write[T <: Tx](value: A1[T], out: DataOutput): Unit
//    def read [T <: Tx](in: DataInput, tx: T)(implicit access: tx.Acc): A1[T]
//  }
//}
