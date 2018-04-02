package de.sciss.lucre.stm

import de.sciss.serial.{DataInput, Serializer}
import de.sciss.lucre.stm.{Var => _Var}

import scala.language.higherKinds

object Base {
  type T [_Tx]        = Base { type Tx = _Tx }
  type TA[_Tx, _Acc]  = Base { type Tx = _Tx; type Acc = _Acc }
}
trait Base {

  type Id // <: Identifier[this.type]
  type Acc
  type Var[A] <: _Var[this.type, A]

  def newId(): Id

  type Tx // <: Tx

//  def self: Tx

  // ---- variables ----

  def newVar[A]    (id: Id, init: A)(implicit serializer: Serializer[Tx, Acc, A]): Var[A]
  def newBooleanVar(id: Id, init: Boolean): Var[Boolean]
  def newIntVar    (id: Id, init: Int    ): Var[Int]
  def newLongVar   (id: Id, init: Long   ): Var[Long]

  def newVarArray[A](size: Int): Array[Var[A]]

  //  /** Creates a new in-memory transactional map for storing and retrieving values based on a mutable's identifier
  //    * as key. If a system is confluently persistent, the `get` operation will find the most recent key that
  //    * matches the search key. Objects are not serialized but kept live in memory.
  //    *
  //    * ID maps can be used by observing views to look up associated view meta data even though they may be
  //    * presented with a more recent access path of the model peer (e.g. when a recent event is fired and observed).
  //    *
  //    * @tparam A         the value type in the map
  //    */
  //  def newInMemoryIDMap[A]: IdentifierMap[Id, Tx, A]

  def readVar[A]    (id: Id, in: DataInput)(implicit serializer: Serializer[Tx, Acc, A]): Var[A]
  def readBooleanVar(id: Id, in: DataInput): Var[Boolean]
  def readIntVar    (id: Id, in: DataInput): Var[Int]
  def readLongVar   (id: Id, in: DataInput): Var[Long]

  def readId(in: DataInput)(implicit acc: Acc): Id

  /** Creates a handle (in-memory) to refresh a stale version of an object, assuming that the future transaction is issued
    * from the same cursor that is used to create the handle, except for potentially having advanced.
    * This is a mechanism that can be used in live views to gain valid access to a referenced object
    * (e.g. self access).
    *
    * @param value         the object which will be refreshed when calling `get` on the returned handle
    * @param serializer    used to write and freshly read the object
    * @return              the handle
    */
  def newHandle[A](value: A)(implicit serializer: Serializer[Tx, Acc, A]): Source[Tx, A]

  // ---- completion ----

  def beforeCommit(fun: Tx => Unit): Unit
  def afterCommit (fun:    => Unit): Unit

  //  // ---- context ----
  //
  //  def use[A](context: S#Context)(fun: => A): A
  //
  //  // ---- events ----
  //
  //  private[lucre] def reactionMap: ReactionMap[S]
  //
  //  // ---- attributes ----
  //
  //  def attrMap(obj: Obj[S]): Obj.AttrMap[S]
}
