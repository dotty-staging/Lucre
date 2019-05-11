//package de.sciss.lucre.expr.impl
//
//import de.sciss.lucre.event.IPush.Parents
//import de.sciss.lucre.event.impl.IEventImpl
//import de.sciss.lucre.event.{IEvent, IPull}
//import de.sciss.lucre.expr.ITrigger
//import de.sciss.lucre.expr.graph.Trig
//import de.sciss.lucre.stm.Sys
//
//trait ITriggerEventImpl[S <: Sys[S]] extends IEventImpl[S, Unit] {
////  private[this] val disposables = Ref(List.empty[Disposable[S#Tx]])
////
////  protected def addDisposable(d: Disposable[S#Tx])(implicit tx: S#Tx): Unit =
////    disposables.transform(d :: _)
//
//  def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit =
//    tr.changed ---> this
//
//  def changed: IEvent[S, Unit] = this
//
//  private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx) : Option[Unit] = {
//    if (pull.isOrigin(this)) Trig.Some
//    else {
//      val p: Parents[S] = pull.parents(this)
//      if (p.exists(pull(_).isDefined)) Trig.Some else None
//    }
//  }
//}
