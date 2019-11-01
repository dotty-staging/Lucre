/*
 *  IPush.scala
 *  (Lucre)
 *
 *  Copyright (c) 2009-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.event

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm.Base
import de.sciss.model.Change

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.collection.immutable.{Map => IMap}

object IPush {
  private[event] def apply[S <: Base[S], A](origin: IEvent[S, A], update: A)
                                           (implicit tx: S#Tx, targets: ITargets[S]): Unit = {
    val push = new Impl(origin, update)
    logEvent("ipush begin")
    push.visitChildren(origin)
    logEvent("ipull begin")
    push.pull()
    logEvent("ipull end")
  }

  type Parents[S <: Base[S]] = Set[IEvent[S, Any]]

  private def NoParents[S <: Base[S]]: Parents[S] = Set.empty[IEvent[S, Any]]

  // private type Visited[S <: Sys[S]] = IMap[Event[S, Any], Parents[S]]
  private final class Reaction[S <: Base[S], +A](update: A, observers: List[Observer[S, A]]) {
    def apply()(implicit tx: S#Tx): Unit =
      observers.foreach(_.apply(update))
  }
  
  private final class Value {
    var state = 0 // 1 = has before, 2 = has now, 4 = has full
    var full  : Option[Any] = None
    var before: Any = _
    var now   : Any = _
    
    def hasBefore : Boolean = (state & 1) != 0
    def hasNow    : Boolean = (state & 2) != 0
    def hasFull   : Boolean = (state & 4) != 0

    def setBefore(v: Any): Unit = {
      before = v
      state |= 1
      if (!hasFull && hasNow) createFull()
    }

    def setNow(v: Any): Unit = {
      now = v
      state |= 2
      if (!hasFull && hasBefore) createFull()
    }
    
    def setFull(v: Option[Any]): Unit = {
      full = v
      state |= 4
      if (!hasNow || !hasBefore) v match {
        case Some(Change(_before, _now)) =>
          before  = _before
          now     = _now
          state  |= 3

        case _ =>
      }
    }
    
    def createFull(): Unit = {
      full = if (before == now) None else Some(Change(before, now))
      state |= 4
    }
  }

  private final class Impl[S <: Base[S]](origin: IEvent[S, Any], val update: Any)
                                        (implicit tx: S#Tx, targets: ITargets[S])
    extends IPull[S] {

    private[this] var pushMap   = IMap(origin -> NoParents[S])
    private[this] var pullMap   = IMap.empty[IEvent[S, Any], Value]

    private[this] var indent    = ""

    @elidable(CONFIG) private[this] def incIndent(): Unit = indent += "  "
    @elidable(CONFIG) private[this] def decIndent(): Unit = indent = indent.substring(2)

    private[this] def addVisited(child: IEvent[S, Any], parent: IEvent[S, Any]): Boolean = {
      val parents = pushMap.getOrElse(child, NoParents)
      logEvent(s"${indent}visit $child  (new ? ${parents.isEmpty})")
      pushMap += ((child, parents + parent))
      parents.isEmpty
    }

    def visitChildren(parent: IEvent[S, Any]): Unit = {
//      val inlet = parent.slot
      incIndent()
      try {
        val childEvents = targets.children(parent) // parent.node._targets.children
        childEvents.foreach { /* case (inlet2, */ child /* ) */ =>
//          if (inlet2 === inlet) {
            visit(child, parent)
//          }
        }

      } finally {
        decIndent()
      }
    }

    def visit(child: IEvent[S, Any], parent: IEvent[S, Any]): Unit =
      if (addVisited(child, parent)) visitChildren(child)

    def contains(source: IEvent[S, Any]): Boolean     = pushMap.contains(source)

    def isOrigin(that  : IEvent[S, Any]): Boolean     = that === origin
    def parents (child : IEvent[S, Any]): Parents[S]  = pushMap.getOrElse(child, NoParents)

    def pull(): Unit = {
      val m0    = currentPull.get()
      val isNew = !m0.contains(tx)
      if (isNew) currentPull.set(m0 + (tx -> this))
      try {
        val reactions: List[Reaction[S, Any]] = pushMap.iterator.flatMap { case (event, _) =>
          val observers: List[Observer[S, Any]] = targets.getEventReactions(event) // tx.reactionMap.getEventReactions(event)
          val hasObs = observers.nonEmpty
          if (hasObs || event.isInstanceOf[Caching]) {
            val opt = apply[Any](event)
            if (hasObs) opt.map(new Reaction(_, observers)) else None
          } else None
        } .toList
        logEvent(s"numReactions = ${reactions.size}")
        reactions.foreach(_.apply())
      } finally {
        if (isNew) currentPull.set(m0)
      }
    }

    def resolve[A]: A = {
      logEvent(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    def resolveChange[A](isNow: Boolean): A = {
      logEvent(s"${indent}resolveChange")
      val ch = update.asInstanceOf[Change[A]]
      if (isNow) ch.now else ch.before
    }

    // Caches pulled values.
    // Noe that we do not check `nonCachedTerms`, implying that
    // `It.Expanded` forbids the use of `pullUpdate` (it throws
    // an exception), and therefore there cannot be a case that
    // circumvents `applyChange` usage.
    def apply[A](source: IEvent[S, A]): Option[A] = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res) =>
            logEvent(s"${indent}pull $source  (new ? false; state = ${res.state})")
            if (res.hasFull) {
              res.full.asInstanceOf[Option[A]]
            } else {
              val v = source.pullUpdate(this)
              res.setFull(v)
              v
            }
          case _ =>
            logEvent(s"${indent}pull $source  (new ? true)")
            val v   = source.pullUpdate(this)
            val res = new Value
            res.setFull(v)
            pullMap += ((source, res))
            v
        }
      } finally {
        decIndent()
      }
    }

//    def TEST_PURGE_CACHE(): Unit = pullMap = pullMap.empty

    private[this] var nonCachedTerms = List.empty[IEvent[S, Any]]
    private[this] var nonCachedPath  = List.empty[IEvent[S, Any]]

    def TEST_NON_CACHED[A](source: IEvent[S, Any])(body: => A): A = {
      val oldTerms  = nonCachedTerms
      val oldPath   = nonCachedPath
      nonCachedTerms  = source :: oldTerms
      nonCachedPath   = Nil
      try {
        body
      } finally {
        nonCachedTerms  = oldTerms
        nonCachedPath   = oldPath
      }
    }

    private def performPullChange[A](source: IChangeEvent[S, A], res: Value, isNow: Boolean): A = {
      if (nonCachedTerms.contains(source)) {
        // Console.err.println(s"erasing non-cached path of size ${nonCachedPath.size} for $source")
        // remove cached values in the current call sequence
        pullMap --= nonCachedPath
        source.pullChange(this, isNow = isNow)
      } else {
        val oldPath = nonCachedPath
        nonCachedPath = source :: oldPath
        val v = try {
          source.pullChange(this, isNow = isNow)
        } finally {
          nonCachedPath = oldPath
        }
        if (isNow) res.setNow(v) else res.setBefore(v)
        v
      }
    }

    // caches pulled values
    def applyChange[A](source: IChangeEvent[S, A], isNow: Boolean): A = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res) =>
            logEvent(s"${indent}pull $source  (new ? false; state = ${res.state})")
            if (isNow) {
              if      (res.hasNow         ) res.now.asInstanceOf[A]
              else if (res.full.isDefined ) res.full.get.asInstanceOf[Change[A]].now
              else {
                performPullChange(source, res, isNow = isNow)
              }
            } else {
              if      (res.hasBefore      ) res.before.asInstanceOf[A]
              else if (res.full.isDefined ) res.full.get.asInstanceOf[Change[A]].before
              else {
                performPullChange(source, res, isNow = isNow)
              }
            }

          case _ =>
            logEvent(s"${indent}pull $source  (new ? true)")
            val res = new Value
            val v   = performPullChange(source, res, isNow = isNow)
            pullMap += ((source, res))
            v
        }
      } finally {
        decIndent()
      }
    }
  }

  private val currentPull = new ThreadLocal[IMap[Any, IPull[_]]]() {
    override def initialValue(): IMap[Any, IPull[_]] = IMap.empty
  }

  def tryPull[S <: Base[S], A](source: IEvent[S, A])(implicit tx: S#Tx): Option[A] =
    currentPull.get().get(tx).flatMap { p =>
      val pc = p.asInstanceOf[IPull[S]]
      if (pc.contains(source)) pc(source) else None
    }
}

trait IPull[S <: Base[S]] {
  /** Assuming that the caller is origin of the event, resolves the update of the given type. */
  def resolve[A]: A

  def resolveChange[A](isNow: Boolean): A

  /** Retrieves the immediate parents from the push phase. */
  def parents(source: IEvent[S, Any]): IPush.Parents[S]

  /** Pulls the update from the given source. */
  def apply[A](source: IEvent[S, A]): Option[A]

  def applyChange[A](source: IChangeEvent[S, A], isNow: Boolean): A

  /** Whether the selector has been visited during the push phase. */
  def contains(source: IEvent[S, Any]): Boolean

  def isOrigin(source: IEvent[S, Any]): Boolean

  /** Marks a region of the pull action as non-caching.
    * This is done by submitting a terminal symbol `source`, typically
    * an instance of `It.Expanded`. When anything within the `body` tries to apply
    * the `source`, all values from events on the call tree will be removed
    * from cache.
    */
  def TEST_NON_CACHED[A](source: IEvent[S, Any])(body: => A): A

//  def TEST_PURGE_CACHE(): Unit
}