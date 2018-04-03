package de.sciss.lucre.stm

import de.sciss.serial.{DataInput, Serializer}

trait Executor[B <: Base[B]] {
  /** Back link to the underlying system. */
  val system: B

  def newId(): B#Id

  // ---- variables ----

  def newVar[A](id: B#Id, init: A)(implicit serializer: Serializer[B#Tx, B#Acc, A]): B#Var[A]
  def newBooleanVar(id: B#Id, init: Boolean): B#Var[Boolean]
  def newIntVar    (id: B#Id, init: Int    ): B#Var[Int]
  def newLongVar   (id: B#Id, init: Long   ): B#Var[Long]

  def newVarArray[A](size: Int): Array[B#Var[A]]

  /** Creates a new in-memory transactional map for storing and retrieving values based on a mutable's identifier
    * as key. If a system is confluently persistent, the `get` operation will find the most recent key that
    * matches the search key. Objects are not serialized but kept live in memory.
    *
    * Id maps can be used by observing views to look up associated view meta data even though they may be
    * presented with a more recent access path of the model peer (e.g. when a recent event is fired and observed).
    *
    * @tparam A         the value type in the map
    */
  def newInMemoryIdMap[A]: IdentifierMap[B#Id, B#Tx, A]

  def readVar[A](id: B#Id, in: DataInput)(implicit serializer: Serializer[B#Tx, B#Acc, A]): B#Var[A]
  def readBooleanVar(id: B#Id, in: DataInput): B#Var[Boolean]
  def readIntVar    (id: B#Id, in: DataInput): B#Var[Int]
  def readLongVar   (id: B#Id, in: DataInput): B#Var[Long]

  def readId(in: DataInput, acc: B#Acc): B#Id

  /** Creates a handle (in-memory) to refresh a stale version of an object, assuming that the future transaction is issued
    * from the same cursor that is used to create the handle, except for potentially having advanced.
    * This is a mechanism that can be used in live views to gain valid access to a referenced object
    * (e.g. self access).
    *
    * @param value         the object which will be refreshed when calling `get` on the returned handle
    * @param serializer    used to write and freshly read the object
    * @return              the handle
    */
  def newHandle[A](value: A)(implicit serializer: Serializer[B#Tx, B#Acc, A]): Source[B#Tx, A]

  // ---- completion ----

  def beforeCommit(fun: B#Tx => Unit): Unit
  def afterCommit (fun:      => Unit): Unit
}