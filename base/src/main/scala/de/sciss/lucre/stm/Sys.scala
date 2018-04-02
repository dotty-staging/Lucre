package de.sciss.lucre.stm

import de.sciss.lucre.stm
import de.sciss.serial.{DataInput, DataOutput, Serializer}

//trait Tx[T <: Tx[T]] extends Tx {
//  final type Self = T
//}

object Sys {
  @inline
  def newVar[A, T <: Tx[T]](tx: T)(id: tx.Id, init: A)
                                 (implicit serializer: Serializer[T, A]): tx.Var[A] =
    tx.newVar[A](id, init)(serializer)
}

trait TestTx extends Tx[TestTx] {
  type Self   = TestTx
  type Id     = TestId
  type Acc    = Unit
  type Var[A] = TestVar[A]

  def newId(): Id = ???

  def newVar[A](id: Id, init: A)(implicit serializer: Serializer[Self, A]): Var[A] =
    new TestVar(id, init)(serializer)

  def readVar[A](id: Id, in: DataInput)(implicit serializer: Serializer[Self, A]): Var[A] = ???

  def readId(in: DataInput)(implicit acc: Acc): Id = ???
}

class TestId extends Identifier[TestTx] {
  def write(out: DataOutput): Unit = ???

  def dispose()(implicit tx: TestTx): Unit = ???
}

class TestVar[A](id: TestId, init: A)(implicit serializer: Serializer[TestTx, A])
  extends stm.Var[TestTx, A] {

  private[this] var STATE = init

  def write(out: DataOutput): Unit = serializer.write(STATE, out)

  def update(v: A)(implicit tx: TestTx): Unit = {
    STATE = v
    serializer.write(STATE, null)
  }

  def apply()(implicit tx: TestTx): A = {
    serializer.read(null, tx)(())
  }

  def dispose()(implicit tx: TestTx): Unit = ()
}