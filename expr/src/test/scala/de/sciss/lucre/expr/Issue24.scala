// seems pretty impossible to allow this

//package de.sciss.lucre.expr
//
//trait Issue24 {
//  import graph._
//  import ExImport._
//
//  {
//    val f = "folder".attr[Folder](Folder())
//    val count: Ex[Seq[Option[Int]]] = f.children.map(_.attr[Int]("proc-count")) // cannot preserve Obj.Attr
//    val reset: Act = count.map(_.set(0))
//  }
//}
