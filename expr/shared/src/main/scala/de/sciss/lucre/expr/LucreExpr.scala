/*
 *  LucreExpr.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package expr

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

//    IntExtensions         .init()
//    LongExtensions        .init()
//    DoubleExtensions      .init()
//    BooleanExtensions     .init()
//    StringExtensions      .init()
//    SpanLikeExtensions    .init()
//    SpanExtensions        .init()

    Expr.Type             .init()
    Ex                    .init()
    graph.Obj             .init()
    graph.Artifact        .init()
    graph.ArtifactLocation.init()
    graph.Folder          .init()
  }
}