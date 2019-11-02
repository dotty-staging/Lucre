/*
 *  IfElse.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.impl.{IChangeEventImpl, IEventImpl}
import de.sciss.lucre.event.{IChangeEvent, IEvent, IPull, ITargets}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

import scala.annotation.tailrec

/** Beginning of a conditional block.
  *
  * @param cond   the condition or predicate for the `then` branch.
  *
  * @see  [[IfThen]]
  */
final case class If(cond: Ex[Boolean]) {
  def Then [A](branch: Ex[A]): IfThen[A] =
    IfThen(cond, branch)

  def Then (branch: Act): IfThenAct =
    IfThenAct(cond, branch)
}

sealed trait Then[+A] /*extends Ex[Option[A]]*/ {
  def cond  : Ex[Boolean]
  def result: Ex[A]

  import de.sciss.lucre.expr.graph.{Else => _Else}

  def Else [B >: A](branch: Ex[B]): Ex[B] =
    _Else(this, branch)

  final def ElseIf (cond: Ex[Boolean]): ElseIf[A] =
    new ElseIf(this, cond)
}

/** A side effecting conditional block. To turn it into a full `if-then-else` construction,
  * call `Else` or `ElseIf`.
  *
  * @see  [[Else]]
  * @see  [[ElseIf]]
  */
final case class IfThen[+A](cond: Ex[Boolean], result: Ex[A]) extends Then[A]

final case class ElseIf[+A](pred: Then[A], cond: Ex[Boolean]) {
  def Then [B >: A](branch: Ex[B]): ElseIfThen[B] =
    ElseIfThen[B](pred, cond, branch)
}

final case class ElseIfThen[+A](pred: Then[A], cond: Ex[Boolean], result: Ex[A])
  extends Then[A]

object Else {
  private type Case[S <: Sys[S], A] = (IExpr[S, Boolean], IExpr[S, A])

  private def gather[S <: Sys[S], A](e: Then[A])(implicit ctx: Context[S],
                                                        tx: S#Tx): List[Case[S, A]] = {
    @tailrec
    def loop(t: Then[A], res: List[Case[S, A]]): List[Case[S, A]] = {
      val condEx  = t.cond  .expand[S]
      val resEx   = t.result.expand[S]
      val res1 = (condEx, resEx) :: res
      t match {
        case hd: ElseIfThen[A] => loop(hd.pred, res1)
        case _ => res1
      }
    }

    loop(e, Nil)
  }

  private final class Expanded[S <: Sys[S], A](cases: List[Case[S, A]], default: IExpr[S, A], tx0: S#Tx)
                                              (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IChangeEventImpl[S, A] {

    cases.foreach { case (cond, res) =>
      cond.changed.--->(this)(tx0)
      res .changed.--->(this)(tx0)
    }
    default.changed.--->(this)(tx0)

    def value(implicit tx: S#Tx): A = {
      @tailrec
      def loop(rem: List[Case[S, A]]): A = rem match {
        case (cond, branch) :: tail =>
          if (cond.value) branch.value else loop(tail)

        case Nil => default.value
      }

      loop(cases)
    }

    def changed: IChangeEvent[S, A] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): A = ???

//    private[lucre] def pullUpdateXXX(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] = {
//      type CaseChange = (Change[Boolean], Change[A])
//
//      // XXX TODO --- this evaluates all branches; we could optimize
//      @tailrec
//      def loop1(rem: List[Case[S, A]], res: List[CaseChange]): List[CaseChange] = rem match {
//        case (cond, branch) :: tail =>
//          val condEvt     = cond  .changed
//          val branchEvt   = branch.changed
//          val condChOpt   = if (pull.contains(condEvt  )) pull(condEvt   ) else None
//          val branchChOpt = if (pull.contains(branchEvt)) pull(branchEvt ) else None
//          val condCh      = condChOpt.getOrElse {
//            val condV = cond.value
//            Change(condV, condV)
//          }
//          val branchCh    = branchChOpt.getOrElse {
//            val branchV = branch.value
//            Change(branchV, branchV)
//          }
//
//          loop1(tail, (condCh, branchCh) :: res)
//
//        case Nil => res.reverse
//      }
//
//      val defaultEvt    = default.changed
//      val defaultChOpt  = if (pull.contains(defaultEvt)) pull(defaultEvt) else None
//      val defaultCh     = defaultChOpt.getOrElse {
//        val defaultV = default.value
//        Change(defaultV, defaultV)
//      }
//
//      @tailrec
//      def loop2(rem: List[CaseChange], isBefore: Boolean): A = rem match {
//        case (cond, branch) :: tail =>
//          val condV = if (isBefore) cond.before else cond.now
//          if (condV) {
//            if (isBefore) branch.before else branch.now
//          } else loop2(tail, isBefore = isBefore)
//
//        case Nil =>
//          if (isBefore) defaultCh.before else defaultCh.now
//      }
//
//      val casesCh     = loop1(cases, Nil)
//      val valueBefore = loop2(casesCh, isBefore = true  )
//      val valueNow    = loop2(casesCh, isBefore = false )
//      val valueCh     = Change(valueBefore, valueNow)
//
//      if (valueCh.isSignificant) Some(valueCh) else None
//    }

    def dispose()(implicit tx: S#Tx): Unit = {
      cases.foreach { case (cond, res) =>
        cond.changed.-/->(this)
        res .changed.-/->(this)
      }
      default.changed.-/->(this)
    }
  }
}
final case class Else[A](pred: Then[A], default: Ex[A]) extends Ex[A] {
  type Repr[S <: Sys[S]] = IExpr[S, A]

  def cond: Ex[Boolean] = true

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val cases     = Else.gather(pred)
    val defaultEx = default.expand[S]

    import ctx.targets
    new Else.Expanded(cases, defaultEx, tx)
  }
}

// ---- Act variants ----

final case class ElseIfAct(pred: ThenAct, cond: Ex[Boolean]) {
  def Then (branch: Act): ElseIfThenAct =
    ElseIfThenAct(pred, cond, branch)
}

object ThenAct {
  private type Case[S <: Sys[S]] = (IExpr[S, Boolean], IAction[S])

  private def gather[S <: Sys[S]](e: ThenAct)(implicit ctx: Context[S],
                                                 tx: S#Tx): List[Case[S]] = {
    @tailrec
    def loop(t: ThenAct, res: List[Case[S]]): List[Case[S]] = {
      val condEx  = t.cond  .expand[S]
      val resEx   = t.result.expand[S]
      val res1 = (condEx, resEx) :: res
      t match {
        case hd: ElseIfThenAct => loop(hd.pred, res1)
        case _ => res1
      }
    }

    loop(e, Nil)
  }

  private final class Expanded[S <: Sys[S]](cases: List[Case[S]])
    extends IAction.Option[S] with IActionImpl[S] {

    def isDefined(implicit tx: S#Tx): Boolean = cases.exists(_._1.value)

    def executeAction()(implicit tx: S#Tx): Unit = executeIfDefined()

    def executeIfDefined()(implicit tx: S#Tx): Boolean = {
      @tailrec
      def loop(rem: List[Case[S]]): Boolean = rem match {
        case (hdCond, hdAct) :: tail =>
          if (hdCond.value) {
            hdAct.executeAction()
            true
          } else {
            loop(tail)
          }

        case Nil => false
      }

      loop(cases)
    }
  }
}

sealed trait ThenAct extends Act {
  type Repr[S <: Sys[S]] = IAction.Option[S]

  def cond  : Ex[Boolean]
  def result: Act

  def Else (branch: Act): Act =
    ElseAct(this, branch)

  final def ElseIf (cond: Ex[Boolean]): ElseIfAct =
    ElseIfAct(this, cond)

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val cases = ThenAct.gather(this)
    new ThenAct.Expanded(cases)
  }
}

object ElseAct {
  private final class Expanded[S <: Sys[S]](pred: IAction.Option[S], default: IAction[S])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit =
      if (!pred.executeIfDefined()) default.executeAction()
  }
}
final case class ElseAct(pred: ThenAct, default: Act) extends Act {
  type Repr[S <: Sys[S]] = IAction[S]

  def cond: Ex[Boolean] = true

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val predEx    = pred    .expand[S]
    val defaultEx = default .expand[S]
    new ElseAct.Expanded(predEx, defaultEx)
  }
}

final case class ElseIfThenAct(pred: ThenAct, cond: Ex[Boolean], result: Act) extends ThenAct

/** A side effecting conditional block. To turn it into a full `if-then-else` construction,
  * call `Else` or `ElseIf`.
  *
  * @see  [[Else]]
  * @see  [[ElseIf]]
  */
final case class IfThenAct(cond: Ex[Boolean], result: Act) extends ThenAct