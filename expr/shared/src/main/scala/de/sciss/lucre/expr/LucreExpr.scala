/*
 *  LucreExpr.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2022 Hanns Holger Rutz. All rights reserved.
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
      import graph.{BinaryOp => BinOp, UnaryOp => UnOp, TernaryOp => TernOp, _}
      Seq[ProductReader[Product]](
        Act, Act.Nop, Act.OrElse, Act.Link,
        Artifact,
        ArtifactLocation,
        Attr, Attr.Set, Attr.Update, Attr.UpdateOption, Attr.WithDefault,
        BinOp,
        BinOp.Plus, BinOp.Minus, BinOp.Times, BinOp.Div, BinOp.ModJ, BinOp.Mod, BinOp.Eq, BinOp.Neq, BinOp.Lt,
        BinOp.Gt, BinOp.Leq, BinOp.Geq, BinOp.Min, BinOp.Max, BinOp.And, BinOp.Or, BinOp.Xor, BinOp.IDiv, BinOp.Lcm,
        BinOp.Gcd, BinOp.RoundTo, BinOp.RoundUpTo, BinOp.Trunc, BinOp.Atan2, BinOp.Hypot, BinOp.Hypotx, BinOp.Pow,
        BinOp.LeftShift, BinOp.RightShift, BinOp.UnsignedRightShift, BinOp.Difsqr, BinOp.Sumsqr, BinOp.Sqrsum,
        BinOp.Sqrdif, BinOp.Absdif, BinOp.Clip2, BinOp.Excess, BinOp.Fold2, BinOp.Wrap2,
        BinOp.RangeExclusive, BinOp.RangeInclusive,
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
        Ex.MapExOption, Ex.MapExSeq, Ex.MapActOption, Ex.MapSeqAct, Ex.FlatMapExOption, Ex.FlatMapExSeq,
        Ex.FlatMapExSeqOption,
        ExOption.Select,
        ExSeq,
        ExSeq.Count, ExSeq.DropWhile, ExSeq.Exists, ExSeq.Filter, ExSeq.FilterNot, ExSeq.Forall, ExSeq.Find,
        ExSeq.FindLast, ExSeq.IndexWhere, ExSeq.Select, ExSeq.SelectFirst, ExSeq.TakeWhile,
        ExTuple2,
        Folder, Folder.Size, Folder.IsEmpty, Folder.NonEmpty, Folder.Children, Folder.Append, Folder.Prepend,
        Folder.Drop, Folder.DropRight, Folder.Clear, Folder.Remove,
        IfThen, IfThenAct, Else, ElseAct, ElseIfThen, ElseIfThenAct,
        It,
        Latch,
        LoadBang,
        Obj.Empty, Obj.Attr, Obj.Attr.Update, Obj.Attr.UpdateOption, Obj.Attr.Set, Obj.As, Obj.Make, Obj.Copy,
        OptionGet,
        PrintLn,
        QuaternaryOp, QuaternaryOp.SeqMkString, QuaternaryOp.SeqPatch,
        QuinaryOp, QuinaryOp.LinLin, QuinaryOp.LinExp, QuinaryOp.ExpLin, QuinaryOp.ExpExp,
        Quote,
        Random, Random.Coin, Random.Until, Random.Range,
        Span, Span.From, Span.Until, Span.All, Span.Void,
        StringFormat,
        TBinaryOp, TBinaryOp.And,
        TernOp,
        TernOp.Clip, TernOp.Fold, TernOp.Wrap, TernOp.StringSlice, TernOp.StringSplit, TernOp.SeqIndexOf,
        TernOp.SeqIndexOfSlice, TernOp.SeqLastIndexOf, TernOp.SeqLastIndexOfSlice, TernOp.SeqPadTo, TernOp.SeqSlice,
        TernOp.SeqSliding, TernOp.SeqStartsWith, TernOp.SeqUpdated,
        TimeStamp, TimeStamp.Update, TimeStamp.Format,
        ToTrig,
        Trig,
        TTBinaryOp, TTBinaryOp.And, TTBinaryOp.Or, TTBinaryOp.Xor,
        UnOp,
        UnOp.Neg, UnOp.Not, UnOp.BitNot, UnOp.Abs, UnOp.ToDouble, UnOp.ToInt, UnOp.ToLong, UnOp.Ceil, UnOp.Floor,
        UnOp.Frac, UnOp.Signum, UnOp.Squared, UnOp.Cubed, UnOp.Sqrt, UnOp.Exp, UnOp.Reciprocal, UnOp.Midicps,
        UnOp.Cpsmidi, UnOp.Midiratio, UnOp.Ratiomidi, UnOp.Dbamp, UnOp.Ampdb, UnOp.Octcps, UnOp.Cpsoct, UnOp.Log,
        UnOp.Log2, UnOp.Log10, UnOp.Sin, UnOp.Cos, UnOp.Tan, UnOp.Asin, UnOp.Acos, UnOp.Atan, UnOp.Sinh, UnOp.Cosh,
        UnOp.Tanh, UnOp.IsPowerOfTwo, UnOp.NextPowerOfTwo, UnOp.IsEven, UnOp.IsOdd,
        UnOp.ToStr, UnOp.OptionSome, UnOp.OptionIsEmpty, UnOp.OptionIsDefined, UnOp.OptionToList,
        UnOp.OptionGet, UnOp.Tuple2_1, UnOp.Tuple2_2, UnOp.Tuple2Swap, UnOp.SeqDistinct, UnOp.SeqHeadOption,
        UnOp.SeqIndices, UnOp.SeqIsEmpty, UnOp.SeqLastOption, UnOp.SeqMaxOption, UnOp.SeqMinOption, UnOp.SeqNonEmpty,
        UnOp.SeqPermutations, UnOp.SeqProduct, UnOp.SeqReverse, UnOp.SeqSize, UnOp.SeqSorted, UnOp.SeqSum,
        UnOp.SeqZipWithIndex, UnOp.SeqIntegrate, UnOp.SeqDifferentiate, UnOp.StringIsEmpty, UnOp.StringNonEmpty,
        UnOp.StringLength, UnOp.StringToIntOption, UnOp.StringToDoubleOption, UnOp.StringToBooleanOption,
        UnOp.SpanLikeIsEmpty, UnOp.SpanLikeNonEmpty, UnOp.SpanLikeClosedOption, UnOp.SpanLikeStartOption,
        UnOp.SpanLikeStopOption, UnOp.SpanLikeLengthOption, UnOp.SpanStart, UnOp.SpanStop, UnOp.SpanLength,
        UnOp.FileParentOption, UnOp.FilePath, UnOp.FileName, UnOp.FileBase, UnOp.FileExtL,
        Var, Var.Set, Var.Update, Var.Inc, Var.Dec,
      )
    })
  }
}