/*
 *  Act.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package expr
package graph

import ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl

object Act extends ProductReader[Act] {
  def apply(xs: Act*): Act = Apply(xs)

  /** Creates an action that can link or loop back to itself.
    * It does so by calling the function `f` with a trigger
    * which is then linked to the output of `f`. This is useful
    * for creating delays and feedback.
    *
    * Example:
    *
    * {{{
    * val metro = Act.link { self =>
    *   Act(
    *     PrintLn("bang"),
    *     Delay(1.0)(self)
    *   )
    * }
    * }}}
    */
  def link(f: Act => Act): Act = {
    val in  = Trig()
    val out = f(in)
    in ---> out
    in
  }

  private final class ExpandedSeq[T <: Txn[T]](xs: Seq[IAction[T]]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      xs.foreach(_.executeAction())
  }

  private final case class Apply(xs: Seq[Act]) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix = "Act" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val xsEx = xs.map(_.expand[T])
      new ExpandedSeq(xsEx)
    }
  }

  object Link extends ProductReader[Link] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Link = {
      require (arity == 2 && adj == 0)
      val _source = in.readTrig()
      val _sink   = in.readAct()
      new Link(_source, _sink)
    }
  }
  final case class Link(source: Trig, sink: Act)
    extends Control {

    override def productPrefix = s"Act$$Link" // serialization

    type Repr[T <: Txn[T]] = IControl[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val tr    = source.expand[T]
      val ac    = sink  .expand[T]
      ac.addSource(tr)
//      val peer  = tr.changed.react { implicit tx => _ => ac.executeAction() }
      IControl.empty // .wrap(peer)
    }
  }

  trait Option extends Act {
    type Repr[T <: Txn[T]] <: IAction.Option[T]
  }

  implicit final class OptionOps(private val in: Act.Option) extends AnyVal {
    def orElse(that: Act): Act = OrElse(in, that)
  }

  private final class ExpandedOrElse[T <: Txn[T]](a: IAction.Option[T], b: IAction[T])/*(implicit ctx: Context[T])*/
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val c = if (a.isDefined) a else b
      c.executeAction()
    }
  }

  object OrElse extends ProductReader[OrElse] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OrElse = {
      require (arity == 2 && adj == 0)
      val _a = in.readProductT[Act.Option]()
      val _b = in.readAct()
      new OrElse(_a, _b)
    }
  }
  final case class OrElse(a: Act.Option, b: Act) extends Act {
    override def productPrefix = s"Act$$OrElse" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ExpandedOrElse(a.expand[T], b.expand[T])
  }

  private final class ExpandedNop[T <: Txn[T]]/*(implicit ctx: Context[T])*/
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = ()
  }

  object Nop extends ProductReader[Nop] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Nop = {
      require (arity == 0 && adj == 0)
      new Nop()
    }
  }
  final case class Nop() extends Act {

    override def productPrefix = s"Act$$Nop" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ExpandedNop
  }

  // ---- serialization ----

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Act = {
    require (arity == 1 && adj == 0)
    val _xs = in.readVec(in.readAct())
    Act(_xs: _*)
  }
}
trait Act extends Lazy {
  type Repr[T <: Txn[T]] <: IAction[T]
}
