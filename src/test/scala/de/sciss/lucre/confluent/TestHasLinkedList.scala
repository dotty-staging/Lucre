package de.sciss.lucre.confluent

import de.sciss.lucre.stm.Mutable
import de.sciss.lucre.stm.impl.{MutableImpl, MutableSerializer}
import de.sciss.serial.{DataInput, DataOutput}

trait TestHasLinkedList {
  class Types[S <: Sys[S]](val s: S) {
    type Sys = S

    object Node {
      implicit object ser extends MutableSerializer[S, Node] {
        def readData(in: DataInput, _id: S#Id)(implicit tx: S#Tx): Node = new Node with MutableImpl[S] {
          val id    = _id
          val name  = in.readUTF()
          val value = tx.readIntVar(id, in)
          val next  = tx.readVar[Option[Node]](id, in)
        }
      }

      def apply(_name: String, init: Int)(implicit tx: S#Tx): Node = new Node with MutableImpl[S] {
        val id    = tx.newId()
        val name  = _name
        val value = tx.newIntVar(id, init)
        val next  = tx.newVar[Option[Node]](id, None)
      }
    }

    trait Node extends Mutable[S#Id, S#Tx] {
      def name: String
      def value: S#Var[Int]
      def next: S#Var[Option[Node]]

      protected def disposeData()(implicit tx: S#Tx): Unit = {
        value.dispose()
        next .dispose()
      }

      protected def writeData(out: DataOutput): Unit = {
        out.writeUTF(name)
        value.write(out)
        next .write(out)
      }

      override def toString = s"Node($name, $id)"
    }

    def toList(next: Option[Node])(implicit tx: S#Tx): List[(String, Int)] = next match {
      case Some(n)  => (n.name, n.value()) :: toList(n.next())
      case _        => Nil
    }

    // tuples of (name, value, id.path)
    def toListId(next: Option[Node])(implicit tx: S#Tx): List[(String, Int, String)] = next match {
      case Some(n)  => (n.name, n.value(), n.id.path.toString) :: toListId(n.next())
      case _        => Nil
    }
  }
}