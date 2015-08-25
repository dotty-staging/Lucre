package de.sciss.lucre.stm
package impl

final class CopyImpl[S <: Sys[S]](implicit tx: S#Tx) extends Copy[S] {
  private[this] sealed trait State
  private[this] final case class Done(elem: Elem[S]) extends State
  private[this] case object Busy extends State

  private[this] val idMap     = tx.newInMemoryIDMap[State]
  private[this] var constMap  = Map.empty[Elem[S], State]

  def provide[Repr <: Obj[S]](in: Repr, out: Repr): Unit = {
    val id = in.id
    if (!idMap.get(id).contains(Busy))
      throw new IllegalStateException(s"Copy.provide must be called during copy process: $in")
    idMap.put(id, Done(out))
  }

  def apply[Repr <: Elem[S]](in: Repr): Repr = in match {
    // d'oh, this gets ugly
    case obj: Identifiable[_] =>
      val id = obj.id.asInstanceOf[S#ID]
      idMap.get(id) match {
        case Some(Done(out)) => out.asInstanceOf[Repr]
        case Some(Busy) => throw new IllegalStateException(s"Cyclic object graph involving $in")
        case None =>
          idMap.put(id, Busy)
          val out = in.copy()(tx, this)
          idMap.put(id, Done(out))
          out.asInstanceOf[Repr]
      }
    case _ =>
      // a pure element with no identifier. we use regular equality
      // XXX TODO -- a bit of a DRY here
      constMap.get(in) match {
        case Some(Done(out)) => out.asInstanceOf[Repr]
        case Some(Busy) => throw new IllegalStateException(s"Cyclic object graph involving $in")
        case None =>
          constMap += (in -> Busy)
          val out = in.copy()(tx, this)
          constMap += (in -> Done(out))
          out.asInstanceOf[Repr]
      }
  }
}
