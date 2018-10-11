# Lucre

[![Build Status](https://travis-ci.org/Sciss/Lucre.svg?branch=master)](https://travis-ci.org/Sciss/Lucre)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre_2.11)

## statement

Lucre is an STM based persistent and reactive system for the Scala programming language. It is the foundation
of the computer music framework [SoundProcesses](https://git.iem.at/sciss/SoundProcesses) and its
interface front-end [Mellite](https://git.iem.at/sciss/Mellite).

Lucre is (C)opyright 2009&ndash;2018 by Hanns Holger Rutz. All rights reserved. 
All modules but the BerkeleyDB bindings are released under 
the [GNU Lesser General Public License](https://git.iem.at/sciss/Lucre/raw/master/licenses/Lucre-License.txt), 
whereas the `bdb` backend module for Berkeley DB JE 5 (itself governed by the Sleepycat License) is released under 
the [GNU General Public License v2+](https://git.iem.at/sciss/Lucre/raw/master/licenses/Lucre-BDB-License.txt), 
and the `bdb6` backend module for Berkeley DB JE 6 (itself governed by the AGPL 3 License) is released under 
the [GNU General Public License v3+](https://git.iem.at/sciss/Lucre/raw/master/licenses/Lucre-BDB6-License.txt). 
The software comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

Further reading:

 - Rutz, H.H., "A Reactive, Confluently Persistent Framework for the Design of Computer Music Systems," in Proceedings
   of the 9th Sound and Music Computing Conference (SMC), Copenhagen 2012.

## requirements / installation

Lucre builds with [sbt](http://www.scala-sbt.org/) against Scala 2.12, 2.11.

## linking to Lucre

Lucre comes with multiple modules:

- `base` introduces `Base` quasi-system type, which may or may not be transactional;
  it contains a `Plain` base system.
- `geom` introduces geometric data types (required by `data`)
- `data` introduces data structures compatible with `base`
- `core` introduces `Sys` system type, extending over `base`, and introducing the
  event reaction layer; it contains in-memory and durable systems.
- `expr` introduces expressions
- `confluent` contains a confluently persistent system
- `bdb` and `bdb6` contain database back-ends (GPL), based on Oracle BerkeleyDB Java Edition 5 (`bdb`) or 6 (`bdb6`)

The following dependency is necessary:

    "de.sciss" %% "lucre" % v

Or just for a selected module:

    "de.sciss" %% "lucre-{module}" % v

Where `{module}` is any of the above names. And for the database backend:

    resolvers += "Oracle Repository" at "http://download.oracle.com/maven"
    
    "de.sciss" %% "lucre-bdb"  % v   // BDB JE v5
    "de.sciss" %% "lucre-bdb6" % v   // BDB JE v6
    
Note that the file format of BDB JE v6 is not backward compatible with v5. Also BDB JE v6 requires Java 1.7, 
whereas BDB v5 works with Java 1.6.

The current version `v` is `"3.10.1"`.

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

