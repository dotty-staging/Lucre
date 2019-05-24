/*
 *  Txn.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.stm

import java.io.Closeable

import de.sciss.lucre.event.ReactionMap

import scala.annotation.tailrec
import scala.concurrent.stm.Txn.ExternalDecider
import scala.concurrent.stm.{InTxn, InTxnEnd, TxnExecutor, TxnLocal, Txn => ScalaTxn}
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

object Txn {
  trait Resource extends Closeable with ExternalDecider

  private[this] final class Decider(var instances: scala.List[Resource])
    extends ExternalDecider {

    def shouldCommit(implicit txn: InTxnEnd): Boolean = instances match {
      case single :: Nil => single.shouldCommit
      case _ => commitAll(instances, Nil)
    }

    @tailrec
    private[this] def commitAll(remain: scala.List[Resource], done: scala.List[Resource])(implicit txn: InTxnEnd): Boolean =
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

  private[this] var allowNesting = false

  def atomic[A](fun: InTxn => A): A = {
    requireEmpty()
    TxnExecutor.defaultAtomic(fun)
  }

  /** Allows to share a transaction between two systems, necessary
    * for a cross-system `Obj.copy` operation.
    */
  def copy[S1 <: Sys[S1], S2 <: Sys[S2], A](fun: (S1#Tx, S2#Tx) => A)
                                           (implicit cursor1: Cursor[S1], cursor2: Cursor[S2]): A = {
    cursor1.step { tx1 =>
      allowNesting = true // allow only for the next cursor step
      try {
        cursor2.step { implicit tx2 =>
          allowNesting = false
          fun(tx1, tx2)
        }
      } finally {
        allowNesting = false
      }
    }
  }
}
trait Txn[S <: Sys[S]] extends Executor[S] with TxnLike {
  def inMemory: S#I#Tx

  // ---- completion ----

  def beforeCommit(fun: S#Tx => Unit): Unit

  // ---- events ----

  private[lucre] def reactionMap: ReactionMap[S]

  // ---- attributes ----

  def attrMap(obj: Obj[S]): Obj.AttrMap[S]
}