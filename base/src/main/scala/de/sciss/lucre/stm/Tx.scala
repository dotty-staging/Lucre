/*
 *  Txn.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import java.io.Closeable

import de.sciss.lucre.stm.{Var => _Var}
import de.sciss.serial.{DataInput, Serializer}

import scala.annotation.tailrec
import scala.concurrent.stm.Txn.ExternalDecider
import scala.concurrent.stm.{InTxn, InTxnEnd, TxnExecutor, TxnLocal, Txn => ScalaTxn}
import scala.language.higherKinds
import scala.util.control.NonFatal

object TxnLike {
  /** Implicitly extracts a Scala STM transaction from a `TxnLike` instance. */
  implicit def peer(implicit tx: TxnLike): InTxn = tx.peer

  /** Implicitly treats a Scala STM transaction as a `TxnLike` instance. */
  implicit def wrap(implicit peer: InTxn): TxnLike = new Wrapped(peer)

  private final class Wrapped(val peer: InTxn) extends TxnLike {
    override def toString: String = peer.toString

    def afterCommit (code: => Unit): Unit = ScalaTxn.afterCommit(_ => code)(peer)
  }
}
/** This is a minimal trait for any type of transactions that wrap an underlying Scala-STM transaction. */
trait TxnLike {
  /** Every transaction has a plain Scala-STM transaction as a peer. This comes handy for
    * setting up custom things like `TxnLocal`, `TMap`, or calling into the hooks of `concurrent.stm.Txn`.
    * It is also needed when re-wrapping the transaction of one system into another.
    */
  def peer: InTxn

  /** Registers a thunk to be executed after the transaction successfully committed. */
  def afterCommit(code: => Unit): Unit

  // will not be able to override this in Txn....
  // def beforeCommit(fun: TxnLike => Unit): Unit
}

object Tx {
  trait Resource extends Closeable with ExternalDecider

  private[this] final class Decider(var instances: List[Resource])
    extends ExternalDecider {

    def shouldCommit(implicit txn: InTxnEnd): Boolean = instances match {
      case single :: Nil => single.shouldCommit
      case _ => commitAll(instances, Nil)
    }

    @tailrec
    private[this] def commitAll(remain: List[Resource], done: List[Resource])(implicit txn: InTxnEnd): Boolean =
      remain match {
        case head :: tail =>
          if (head.shouldCommit) {
            commitAll(tail, head :: done)
          } else {
            if (done.nonEmpty) {
              Console.err.println(s"Resource $head failed to commit transaction.")
              done.foreach { r =>
                Console.err.println(s"Closing $r as a precaution. The system must be re-opened prior to further use.")
                try {
                  r.close()
                } catch {
                  case NonFatal(e) => e.printStackTrace()
                }
              }
            }
            false
          }

        case _ => true
      }
  }

  private[this] val decider = TxnLocal[Decider]()

  private[lucre] def addResource(resource: Resource)(implicit tx: InTxn): Unit =
    if (decider.isInitialized) {
      decider().instances ::= resource
    } else {
      val d = new Decider(resource :: Nil)
      ScalaTxn.setExternalDecider(d)
      decider() = d
    }

  private[lucre] def requireEmpty(): Unit =
    if (!allowNesting && ScalaTxn.findCurrent.isDefined)
      throw new IllegalStateException("Nested transactions are not supported by this system.")

  private[this] val allowNesting = false

  def atomic[A](fun: InTxn => A): A = {
    requireEmpty()
    TxnExecutor.defaultAtomic(fun)
  }
}

trait Tx extends TxnLike {

//  /** Back link to the underlying system. */
//  val system: S
//
//  def inMemory: S#I#Tx
  
  type Id <: Identifier[this.type]
  type Acc
  type Var[A] <: _Var[this.type, A]

  def newId(): Id

  type Self <: Tx

  def self: Self

  // ---- variables ----

  def newVar[A]    (id: Id, init: A)(implicit serializer: Serializer[Self, A]): Var[A]
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
//  def newInMemoryIDMap[A]: IdentifierMap[Id, Self, A]

  def readVar[A]    (id: Id, in: DataInput)(implicit serializer: Serializer[Self, A]): Var[A]
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
  def newHandle[A](value: A)(implicit serializer: Serializer[Self, A]): Source[Self, A]

  // ---- completion ----

  def beforeCommit(fun: Self  => Unit): Unit
  def afterCommit (fun:       => Unit): Unit

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