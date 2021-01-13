/*
 *  LucreExpr.scala
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

import de.sciss.lucre.expr.ExElem.ProductReader
import de.sciss.lucre.expr.graph.Ex

object LucreExpr {
  def init(): Unit = {
    IntObj                .init()
    LongObj               .init()
    DoubleObj             .init()
    BooleanObj            .init()
    StringObj             .init()
    SpanLikeObj           .init()
    SpanObj               .init()
    IntVector             .init()
    DoubleVector          .init()

    ListObj               .init()
    Folder                .init()
    MapObj                .init()
    Artifact              .init()
    ArtifactLocation      .init()

    BiPin                 .init()
    BiGroup               .init()

    IntExtensions         .init()
    LongExtensions        .init()
    DoubleExtensions      .init()
    BooleanExtensions     .init()
    StringExtensions      .init()
    SpanLikeExtensions    .init()
    SpanExtensions        .init()

    Expr.Type             .init()
    Ex                    .init()
    graph.Obj             .init()
    graph.Artifact        .init()
    graph.ArtifactLocation.init()
    graph.Folder          .init()

    ExElem.addProductReaderSq({
      import graph.{BinaryOp => BinOp, UnaryOp => UnOp, _}
      Seq[ProductReader[Product]](
        Act, Act.Nop, Act.OrElse, Act.Link,
        Artifact,
        ArtifactLocation,
        Attr, Attr.Set, Attr.Update, Attr.WithDefault,
        BinOp,
        BinOp.Plus, BinOp.Minus, BinOp.Times, BinOp.Div, BinOp.ModJ, BinOp.Mod, BinOp.Eq, BinOp.Neq, BinOp.Lt,
        BinOp.Gt, BinOp.Leq, BinOp.Geq, BinOp.Min, BinOp.Max, BinOp.And, BinOp.Or, BinOp.Xor, BinOp.IDiv, BinOp.Lcm,
        BinOp.Gcd, BinOp.RoundTo, BinOp.RoundUpTo, BinOp.Trunc, BinOp.Atan2, BinOp.Hypot, BinOp.Hypotx, BinOp.Pow,
        BinOp.LeftShift, BinOp.RightShift, BinOp.UnsignedRightShift, BinOp.Difsqr, BinOp.Sumsqr, BinOp.Sqrsum,
        BinOp.Sqrdif, BinOp.Absdif, BinOp.Clip2, BinOp.Excess, BinOp.Fold2, BinOp.Wrap2,
        BinOp.OptionContains, BinOp.OptionGetOrElse, BinOp.OptionOrElse,
        BinOp.SeqAppended, BinOp.SeqApply, BinOp.SeqApplyOption, BinOp.SeqConcat, BinOp.SeqContains, BinOp.SeqDiff,
        BinOp.SeqDrop, BinOp.SeqDropRight, BinOp.SeqEndsWith, BinOp.SeqGrouped, BinOp.SeqIndexOf,
        BinOp.SeqIndexOfSlice, BinOp.SeqIntersect, BinOp.SeqIsDefinedAt, BinOp.SeqLastIndexOf,
        BinOp.SeqLastIndexOfSlice, BinOp.SeqPrepended, BinOp.SeqSameElements, BinOp.SeqSplitAt, BinOp.SeqTake,
        BinOp.SeqTakeRight, BinOp.SeqZip,
        BinOp.StringConcat, BinOp.StringContains, BinOp.StringStartsWith,
        BinOp.StringEndsWith, BinOp.StringIndexOf, BinOp.StringLastIndexOf, BinOp.StringTake, BinOp.StringDrop,
        BinOp.SpanLikeClip, BinOp.SpanLikeShift, BinOp.SpanLikeContains,
        BinOp.SpanLikeOverlaps, BinOp.SpanLikeTouches, BinOp.SpanLikeUnion, BinOp.SpanLikeIntersect,
        BinOp.FileReplaceExt, BinOp.FileReplaceName, BinOp.FileChild,
        Changed,
        Debug.PrintNow,
        Edit, Edit.Apply, Edit.Named,
        ExSeq,
        ExSeq.Count, ExSeq.DropWhile, ExSeq.Exists, ExSeq.Filter, ExSeq.FilterNot, ExSeq.Forall, ExSeq.Find,
        ExSeq.FindLast, ExSeq.IndexWhere, ExSeq.Select, ExSeq.SelectFirst, ExSeq.TakeWhile,
        ExTuple2,
        It,
        LoadBang,
        OptionGet,
        PrintLn,
        Quote,
        ToTrig,
        UnOp,
        UnOp.ToStr,
        UnOp.OptionSome, UnOp.OptionIsEmpty, UnOp.OptionIsDefined, UnOp.OptionToList, UnOp.OptionGet,
        Var, Var.Set, Var.Update,
      )
    })
  }
}