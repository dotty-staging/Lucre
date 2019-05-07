/*
 *  Graph.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.{Control, It}
import de.sciss.lucre.expr.impl.{ExElem, GraphBuilderMixin, GraphSerializerMixin}
import de.sciss.lucre.stm.Sys
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}

object Graph {
  trait Builder {
    def addControl  (c: Control): Unit
    def putProperty (c: Control, key: String, value: Any): Unit

    def allocToken[A](): It[A]
  }

  /** This is analogous to `SynthGraph.Builder` in ScalaCollider. */
  def builder: Builder = builderRef.get()

  private[this] val builderRef: ThreadLocal[Builder] = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  private[this] object BuilderDummy extends Builder {
    private def outOfContext: Nothing = sys.error("Out of context")

    def addControl(c: Control): Unit = ()

    def putProperty(c: Control, key: String, value: Any): Unit = ()

    def allocToken[A](): It[A] = outOfContext
  }

  def apply(thunk: => Any): Graph = {
    val b = new BuilderImpl
    use(b) {
      thunk
      b.build()
    }
  }

  def use[A](b: Builder)(body: => A): A = {
    val old = builderRef.get()
    builderRef.set(b)
    try {
      body
    } finally {
      builderRef.set(old)
    }
  }

  private[this] final class BuilderImpl extends GraphBuilderMixin {
    override def toString = s"lucre.expr.Graph.Builder@${hashCode.toHexString}"
  }

  implicit object serializer extends ImmutableSerializer[Graph] with GraphSerializerMixin {
    private final val SER_VERSION = 0x4378  // "Cx"

    def write(g: Graph, out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      val ref = null: ExElem.RefMapOut
      val cx = g.controls
      writeControls(cx, out, ref)
    }

    def read(in: DataInput): Graph = {
      val cookie = in.readShort()
      require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
      val ref = new ExElem.RefMapIn
      val cx = readControls(in, ref)
      Graph(cx)
    }
  }

  private final class ExpandedImpl[S <: Sys[S]](controls: ISeq[IControl[S]])
    extends IControl[S] {

    def initControl()(implicit tx: S#Tx): Unit =
      controls.foreach(_.initControl())

    def dispose()(implicit tx: S#Tx): Unit =
      controls.foreach(_.dispose())
  }

  val empty: Graph = Graph(Vector.empty)

  def apply(controls: Vec[Control.Configured]): Graph = Impl(controls)

  private final case class Impl(controls: Vec[Control.Configured]) extends Graph {

    override def productPrefix: String = "Graph"

    def expand[S <: Sys[S]](implicit tx: S#Tx, ctx: Context[S]): IControl[S] = {
      if (controls.isEmpty) IControl.empty[S] else {
        val disposables = ctx.withGraph(this) {
          controls.map(_.control.expand[S])
        }
        new Graph.ExpandedImpl[S](disposables)
      }
    }

//    def expand[S <: Sys[S]](self: Option[Obj[S]] = None)
//                           (implicit tx: S#Tx, workspace: Workspace[S], cursor: Cursor[S]): IControl[S] = {
//      implicit val ctx: ExContext[S] = ExContext(this, self.map(tx.newHandle(_)))
//        ...
//    }
  }
}
trait Graph {
  def controls: Vec[Control.Configured]

  def expand[S <: Sys[S]](implicit tx: S#Tx, ctx: Context[S]): IControl[S]
}