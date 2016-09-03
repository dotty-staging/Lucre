lazy val baseName         = "Lucre"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "3.3.2-SNAPSHOT"

lazy val sleepyVersion5   = "5.0.104" // = Berkeley DB Java Edition; note: version 6 requires Java 7
lazy val sleepyVersion6   = "6.2.7"
lazy val serialVersion    = "1.0.2"
lazy val scalaSTMVersion  = "0.7"
lazy val scalaTestVersion = "3.0.0"
lazy val modelVersion     = "0.3.2"
lazy val fingerVersion    = "1.5.2"
lazy val spanVersion      = "1.3.1"
lazy val numbersVersion   = "0.1.1"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "Extension of Scala-STM, adding optional durability layer, and providing API for confluent and reactive event layers",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.11.8",
  crossScalaVersions  := Seq("2.11.8", "2.10.6"),
  scalacOptions      ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture"),
  scalacOptions      ++= {
    if (loggingEnabled && isSnapshot.value) Nil else Seq("-Xelide-below", "INFO")     // elide debug logging!
  },
  testOptions in Test += Tests.Argument("-oDF"),   // ScalaTest: durations and full stack traces
  parallelExecution in Test := false,
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
) ++ publishSettings

lazy val lgpl = "LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")
lazy val gpl2 = "GPL v2+"    -> url("http://www.gnu.org/licenses/gpl-2.0.txt" )
lazy val gpl3 = "GPL v3+"    -> url("http://www.gnu.org/licenses/gpl-3.0.txt" )

// i.e. root = full sub project. if you depend on root, will draw all sub modules.
lazy val root: Project = Project(id = baseNameL, base = file("."))
  .aggregate(core, expr, confluent, bdb, bdb6)
  .dependsOn(core, expr, confluent, bdb /* , bdb6 */)
  .settings(commonSettings)
  .settings(
    licenses := Seq(gpl2),
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false  // there are no sources
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    licenses := Seq(lgpl),
    libraryDependencies ++= Seq(
      "org.scala-stm" %% "scala-stm" % scalaSTMVersion,
      "de.sciss"      %% "serial"    % serialVersion
    ),
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) {
        case (k, opt) => k -> opt.get
      },
      BuildInfoKey.map(licenses) {
        case (_, Seq((lic, _))) => "license" -> lic
      }
    ),
    buildInfoPackage := "de.sciss.lucre"
  )

lazy val expr = Project(id = s"$baseNameL-expr", base = file("expr"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    licenses := Seq(lgpl),
    libraryDependencies ++= Seq(
      "de.sciss" %% "model"   % modelVersion,
      "de.sciss" %% "span"    % spanVersion,
      "de.sciss" %% "numbers" % numbersVersion
    )
  )

lazy val confluent = Project(id = s"$baseNameL-confluent", base = file("confluent"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    licenses := Seq(lgpl),
    libraryDependencies ++= Seq(
      "de.sciss" %% "fingertree" % fingerVersion
    )
  )

lazy val bdb = Project(id = s"$baseNameL-bdb", base = file("bdb"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    licenses := Seq(gpl2),
    resolvers += "Oracle Repository" at "http://download.oracle.com/maven", // required for sleepycat
    libraryDependencies += "com.sleepycat" % "je" % sleepyVersion5
  )

lazy val bdb6 = Project(id = s"$baseNameL-bdb6", base = file("bdb6"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    licenses := Seq(gpl3),
    resolvers += "Oracle Repository" at "http://download.oracle.com/maven",
    libraryDependencies += "com.sleepycat" % "je" % sleepyVersion6
  )

lazy val loggingEnabled = false  // only effective for snapshot versions

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
<scm>
  <url>git@github.com:Sciss/{baseName}.git</url>
  <connection>scm:git:git@github.com:Sciss/{baseName}.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>
  }
)
