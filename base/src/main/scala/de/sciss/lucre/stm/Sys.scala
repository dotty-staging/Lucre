package de.sciss.lucre.stm

import de.sciss.lucre.stm
import de.sciss.serial.{DataInput, Serializer}

object Sys {
  @inline
  def newVar[A, T <: Txn](tx: T)(id: tx.Id, init: A)
                         (implicit serializer: Serializer[T, A]): tx.Var[A] =
    tx.newVar[A](id, init)
}

trait TestTxn extends Txn {
  type Self   = TestTxn
  type Id     = Identifier[TestTxn]
  type Acc    = Unit
  type Var[A] = stm.Var[TestTxn, A]

  def newId(): Id = ???

  def newVar[A](id: Id, init: A)(implicit serializer: Serializer[this.type, A]): Var[A] = ???

  def readVar[A](id: Id, in: DataInput)(implicit serializer: Serializer[this.type, A]): Var[A] = ???

  def readId(in: DataInput)(implicit acc: Acc): Id = ???

}