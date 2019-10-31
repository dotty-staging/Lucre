package de.sciss.lucre.stm

// This should compile without erasure warning for the match.
trait MatchFormCompile {
  def test[S <: Base[S]](m: Map[String, Form[S]]): Boolean = m.get("foo").exists {
    case _: Obj[S]  => true
    case _          => false
  }
}
