lazy val baseName         = "Lucre"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "3.17.7-SNAPSHOT"
lazy val mimaVersion      = "3.17.0"

lazy val deps = new {
  val base = new {
    val serial        = "1.1.3-SNAPSHOT"
  }
  val core = new {
    val equal         = "0.1.4"
    val model         = "0.3.4"
    val scalaSTM      = "0.9.1"
  }
  val expr = new {
    def equal: String = core.equal
    val fileUtil      = "1.1.5-SNAPSHOT"
    val numbers       = "0.2.1-SNAPSHOT"
    val span          = "1.4.3"
  }
  val confluent = new {
    val finger        = "1.5.4"
  }
  val bdb = new {
    val sleepy7       = "7.5.11"  // Apache // Java 8+ required
  }
  val test = new {
    val scalaTest     = "3.2.0"
  }
}

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "Extension of Scala-STM, adding optional durability layer, and providing API for confluent and reactive event layers",
  homepage            := Some(url(s"https://git.iem.at/sciss/$baseName")),
  scalaVersion        := "2.13.3",
  crossScalaVersions  := Seq("0.24.0-RC1", "2.13.3", "2.12.12"),
  scalacOptions      ++= Seq(
    "-Xlint", "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xsource:2.13"
  ),
  scalacOptions in (Compile, compile) ++= 
    (if (scala.util.Properties.isJavaAtLeast("9") && scalaVersion.value.startsWith("2.")) Seq("-release", "8") else Nil), // JDK >8 breaks API; skip scala-doc
  scalacOptions      ++= {
    if (loggingEnabled && isSnapshot.value) Nil else Seq("-Xelide-below", "INFO")     // elide debug logging!
  },
  testOptions in Test += Tests.Argument("-oDF"),   // ScalaTest: durations and full stack traces
  parallelExecution in Test := false,
  libraryDependencies += {
    "org.scalatest" %% "scalatest" % deps.test.scalaTest % Test
  }
) ++ publishSettings

lazy val agpl = "AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")

// i.e. root = full sub project. if you depend on root, will draw all sub modules.
lazy val root = project.withId(baseNameL).in(file("."))
  .aggregate(base, geom, adjunct, data, core, expr, confluent, bdb)
  .dependsOn(base, geom, adjunct, data, core, expr, confluent, bdb)
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    publishArtifact in (Compile, packageBin) := false, // there are no binaries
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false, // there are no sources
    mimaFailOnNoPrevious := false
  )

lazy val base = project.withId(s"$baseNameL-base").in(file("base"))
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    libraryDependencies ++= Seq(
      "de.sciss" %% "serial" % deps.base.serial
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-base" % mimaVersion)
  )

lazy val geom = project.withId(s"$baseNameL-geom").in(file("geom"))
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    libraryDependencies ++= Seq(
      "de.sciss" %% "serial" % deps.base.serial
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-geom" % mimaVersion)
  )

lazy val adjunct = project.withId(s"$baseNameL-adjunct").in(file("adjunct"))
  .dependsOn(base)
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    libraryDependencies ++= Seq(
      "de.sciss" %% "numbers" % deps.expr.numbers
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-adjunct" % mimaVersion)
  )

lazy val data = project.withId(s"$baseNameL-data").in(file("data"))
  .dependsOn(base, geom)
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-data" % mimaVersion)
  )

lazy val core = project.withId(s"$baseNameL-core").in(file("core"))
  .dependsOn(data)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    libraryDependencies ++= Seq(
      "de.sciss"      %% "equal"     % deps.core.equal % Provided,
      "de.sciss"      %% "model"     % deps.core.model,
      "org.scala-stm" %% "scala-stm" % deps.core.scalaSTM
    ),
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) {
        case (k, opt) => k -> opt.get
      },
      BuildInfoKey.map(licenses) {
        case (_, Seq((lic, _))) => "license" -> lic
      }
    ),
    buildInfoPackage := "de.sciss.lucre",
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )

lazy val expr = project.withId(s"$baseNameL-expr").in(file("expr"))
  .dependsOn(core, adjunct)
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    libraryDependencies ++= Seq(
      "de.sciss" %% "equal"     % deps.expr.equal % Provided,
      "de.sciss" %% "fileutil"  % deps.expr.fileUtil,
      "de.sciss" %% "span"      % deps.expr.span,
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-expr" % mimaVersion)
  )

lazy val confluent = project.withId(s"$baseNameL-confluent").in(file("confluent"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    libraryDependencies ++= Seq(
      "de.sciss" %% "fingertree" % deps.confluent.finger
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-confluent" % mimaVersion)
  )

lazy val bdb = project.withId(s"$baseNameL-bdb").in(file("bdb"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    licenses := Seq(agpl),
    libraryDependencies += "de.sciss" % "bdb-je" % deps.bdb.sleepy7,
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-bdb" % mimaVersion)
  )

lazy val loggingEnabled = true  // only effective for snapshot versions

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
  <url>git@git.iem.at:sciss/{baseName}.git</url>
  <connection>scm:git:git@git.iem.at:sciss/{baseName}.git</connection>
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
