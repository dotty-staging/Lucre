package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys
import de.sciss.serial.{Serializer, DataOutput, DataInput}

trait Type { def typeID: Int }

trait ExprType[A] extends Type {

  // ---- abstract ----

  def readValue (          in : DataInput ): A
  def writeValue(value: A, out: DataOutput): Unit

  implicit def serializer   [S <: Sys[S]]: Serializer[S#Tx, S#Acc, Expr    [S, A]]
  implicit def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Expr.Var[S, A]]

  // ---- public ----

  def newConst [S <: Sys[S]](value: A): Expr.Const[S, A]
  def newVar   [S <: Sys[S]](init: Expr[S, A])(implicit tx: S#Tx): Expr.Var[S, A]

  def readConst[S <: Sys[S]](in: DataInput): Expr.Const[S, A]
  def readVar  [S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Expr.Var[S, A]
}