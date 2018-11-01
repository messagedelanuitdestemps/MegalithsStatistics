import ReleaseTransformations._
import sbtassembly.MergeStrategy

name := "nuitdestemps"
organization := "org.nuitdestemps"
scalaVersion := "2.11.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")


// project dependencies
libraryDependencies ++= {
  Seq(
    //"com.thenewmotion.akka" %% "akka-rabbitmq" % "2.3",
     "ch.qos.logback" % "logback-classic" % "1.1.7"
    ,"com.github.jpbetz" % "subspace" % "0.1.0"
    //,"org.eclipse.tycho" % "org.eclipse.jdt.core" % "3.9.0.v20130313-2254" 
    //    "com.madhukaraphatak" %% "java-sizeof" % "0.1"
  )
}

// credentials used to publish artifact
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")


// disable publishing the main jar produced by `package`
publishArtifact in(Compile, packageBin) := false

// main class for default package phase
mainClass in(Compile, run) := Some("org.messagedelanuitdestemps.anglestest.MenhirCombis")

// merge strategy for assembly (2 files or class with same name + location)
val customMergeStrategy: String => MergeStrategy = {
  case x if Assembly.isConfigFile(x) =>
    MergeStrategy.concat
  case PathList(ps@_*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
    MergeStrategy.rename
  case PathList("META-INF", xs@_*) =>
    (xs map {
      _.toLowerCase
    }) match {
      case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
        MergeStrategy.discard
      case ps@(x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
        MergeStrategy.discard
      case "plexus" :: xs =>
        MergeStrategy.discard
      case "spring.tooling" :: xs =>
        MergeStrategy.discard
      case "services" :: xs =>
        MergeStrategy.filterDistinctLines
      case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
        MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.discard//deduplicate
    }
  case "asm-license.txt" | "overview.html" =>
    MergeStrategy.discard
  case _ => MergeStrategy.first //deduplicate
}

excludeFilter in unmanagedSources := HiddenFileFilter || "init.scala"

assemblyMergeStrategy in assembly := customMergeStrategy
// dont add scala version in the end of the artifact name
crossPaths := false

// publish fat jar without classifier
artifact in(Compile, assembly) := {
  val art = (artifact in(Compile, assembly)).value
  art.copy(`classifier` = Some(""))
}
addArtifact(artifact in(Compile, assembly), assembly)

releaseIgnoreUntrackedFiles := true
// assembly main class
mainClass in assembly := Some("org.messagedelanuitdestemps.anglestest.MenhirCombis")

//release process -> dont publish artifact
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

Revolver.settings
