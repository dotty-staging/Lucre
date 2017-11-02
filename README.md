# Lucre

[![Build Status](https://travis-ci.org/Sciss/Lucre.svg?branch=master)](https://travis-ci.org/Sciss/Lucre)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre_2.11)

## statement

Lucre is an STM based persistent and reactive system for the Scala programming language. This is the next
generation of the project that in version 2 was split into several sub-projects
([LucreSTM](https://github.com/Sciss/LucreSTM), [LucreData](https://github.com/Sciss/LucreData),
[LucreConfluent](https://github.com/Sciss/LucreConfluent)).

Lucre is (C)opyright 2009&ndash;2017 by Hanns Holger Rutz. All rights reserved. 
All modules but the BerkeleyDB bindings are released under 
the [GNU Lesser General Public License](https://raw.github.com/Sciss/LucreSTM/master/licenses/Lucre-License.txt), 
whereas the `bdb` backend module for Berkeley DB JE 5 (itself governed by the Sleepycat License) is released under 
the [GNU General Public License v2+](https://raw.github.com/Sciss/LucreSTM/master/licenses/LucreSTM-BDB-License.txt), 
and the `bdb6` backend module for Berkeley DB JE 6 (itself governed by the AGPL 3 License) is released under 
the [GNU General Public License v3+](https://raw.github.com/Sciss/LucreSTM/master/licenses/LucreSTM-BDB6-License.txt). 
The software comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

Further reading:

 - Rutz, H.H., "A Reactive, Confluently Persistent Framework for the Design of Computer Music Systems," in Proceedings of the 9th Sound and Music Computing Conference (SMC), Copenhagen 2012.

## requirements / installation

Lucre builds with [sbt](http://www.scala-sbt.org/) 0.13 against Scala 2.12, 2.11.

## linking to Lucre

Lucre comes with multiple modules, `core`, `durable`, `confluent`, `data`, `expr`, `bdb`, `bdb6`. 
The `bdb` module adds a durable store implementation based on Oracle BerkeleyDB Java Edition 5, `bdb6` uses BDB JE 6.

The following dependency is necessary:

    "de.sciss" %% "lucre" % v

Or just for a selected module:

    "de.sciss" %% "lucrestm-{module}" % v

Where `{module}` is any of the above names. And for the database backend:

    resolvers += "Oracle Repository" at "http://download.oracle.com/maven"
    
    "de.sciss" %% "lucre-bdb"  % v   // BDB JE v5
    "de.sciss" %% "lucre-bdb6" % v   // BDB JE v6
    
Note that the file format of BDB JE v6 is not backward compatible with v5. Also BDB JE v6 requires Java 1.7, 
whereas BDB v5 works with Java 1.6.

The current version `v` is `"3.5.0"`.

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

