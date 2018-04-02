package de.sciss.sandbox

import de.sciss.lucre.stm.Txn
import de.sciss.serial.{DataInput, DataOutput}

import scala.language.higherKinds

object SerTest {
  trait Serializer[A[_ <: Txn]] {
    type A1[T <: Txn] = A[T]

    def write[T <: Txn](value: A1[T], out: DataOutput): Unit
    def read [T <: Txn](in: DataInput, tx: T)(implicit access: tx.Acc): A1[T]
  }

  trait Foo[T <: Txn]

  trait FooSer extends Serializer[Foo] {
    def write[T <: Txn](foo: Foo[T], out: DataOutput): Unit
    def read [T <: Txn](in: DataInput, tx: T)(implicit access: tx.Acc): Foo[T]
  }

  object ImmutableSerializer {
    type NonTxn[A]
  }
  trait ImmutableSerializer[B] extends Serializer[({type A[_ <: Txn] = B})#A] {
    def write[T <: Txn](value: A1[T], out: DataOutput): Unit
    def read [T <: Txn](in: DataInput, tx: T)(implicit access: tx.Acc): A1[T]
  }
}
