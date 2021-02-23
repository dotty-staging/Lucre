# Lucre

[![Build Status](https://github.com/Sciss/Lucre/workflows/Scala%20CI/badge.svg?branch=main)](https://github.com/Sciss/Lucre/actions?query=workflow%3A%22Scala+CI%22)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre-core_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucre-core_2.13)

## statement

Lucre is an STM based persistent and reactive system for the Scala programming language. It is the foundation
of the computer music framework [SoundProcesses](https://git.iem.at/sciss/SoundProcesses) and its
interface front-end [Mellite](https://git.iem.at/sciss/Mellite).

Lucre is (C)opyright 2009&ndash;2021 by Hanns Holger Rutz. All rights reserved. 
All modules are released under 
the [GNU Affero General Public License](https://git.iem.at/sciss/Lucre/raw/main/LICENSE) v3+.
The software comes with absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`.

Further reading:

 - Rutz, H.H., "A Reactive, Confluently Persistent Framework for the Design of Computer Music Systems," in Proceedings
   of the 9th Sound and Music Computing Conference (SMC), Copenhagen 2012.

## requirements / installation

Lucre builds with [sbt](http://www.scala-sbt.org/) against Scala 2.12, 2.13, Dotty (JVM) and Scala 2.13 (JS).
The last version to support Scala 2.11 was v3.13.1.

__N.B.__: On Scala.js, currently the types `Artifact` and `ArtifactLocation` are missing, as there is no notion of
the underlying `java.io.File`. In the future, we might change the API to support more generic paths or URIs instead
of plain files. Furthermore, the package `bdb` is not available on Scala.js, meaning there is currently no
persistent database back-end available, and you will have to stick to `InMemory` systems. `Ex`
serialization will currently fail at link time due to the dependency on reflection which is not supported under
Scala.js. The currently published version for Scala.js should thus be regarded as aiding experiments with Scala.js
only.

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

The current version `v` is `"4.4.3"`.

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

