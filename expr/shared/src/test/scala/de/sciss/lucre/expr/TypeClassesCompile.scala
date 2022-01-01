package de.sciss.lucre.expr

import de.sciss.lucre.expr.graph.Obj

trait TypeClassesCompile {
  implicitly[Obj.CanMake[Int]]
  implicitly[Obj.Source [Int]]
}
