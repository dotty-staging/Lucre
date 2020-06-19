# Lucre

[![Build Status](https://travis-ci.org/Sciss/Lucre.svg?branch=main)](https://travis-ci.org/Sciss/Lucre)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre_2.13)

## statement

Lucre is an STM based persistent and reactive system for the Scala programming language. It is the foundation
of the computer music framework [SoundProcesses](https://git.iem.at/sciss/SoundProcesses) and its
interface front-end [Mellite](https://git.iem.at/sciss/Mellite).

Lucre is (C)opyright 2009&ndash;2020 by Hanns Holger Rutz. All rights reserved. 
All modules are released under 
the [GNU Affero General Public License](https://git.iem.at/sciss/Lucre/raw/main/LICENSE).
The software comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

Further reading:

 - Rutz, H.H., "A Reactive, Confluently Persistent Framework for the Design of Computer Music Systems," in Proceedings
   of the 9th Sound and Music Computing Conference (SMC), Copenhagen 2012.

## requirements / installation

Lucre builds with [sbt](http://www.scala-sbt.org/) against Scala 2.13, 2.12 (last version to support Scala 2.11 is 3.13.1).

## linking to Lucre

Lucre comes with multiple modules:

- `adjunct` introduces a type-class API with fast serialization.
- `base` introduces `Base` quasi-system type, which may or may not be transactional;
  it contains a `Plain` base system.
- `geom` introduces geometric data types (required by `data`)
- `data` introduces data structures compatible with `base`
- `core` introduces `Sys` system type, extending over `base`, and introducing the
  event reaction layer; it contains in-memory and durable systems.
- `expr` introduces expressions
- `confluent` contains a confluently persistent system
- `bdb` contains a database back-end based on Oracle BerkeleyDB Java Edition 7.

The following dependency is necessary:

    "de.sciss" %% "lucre" % v

Or just for a selected module:

    "de.sciss" %% "lucre-{module}" % v

Where `{module}` is any of the above names.

The current version `v` is `"3.17.1"`.

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

