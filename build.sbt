name := "equate"
description := "assert equality; diff observed vs. expected upon failure"
versionScheme := Some("semver-spec")
version := "0.3.0"
organization := "io.github.jonaskoelker"

scalaVersion := "2.13.6"
crossScalaVersions := Seq("2.11.12", "2.12.10", scalaVersion.value)

licenses := Seq(
  "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
)

homepage := Some(url("https://github.com/jonaskoelker/equate"))
startYear := Some(2021)

Compile / scalaSource := baseDirectory.value / "src"
Test / scalaSource := baseDirectory.value / "test"

scalacOptions := Seq(
  "-Xlint",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-encoding", "UTF-8",
  "-Xlog-reflective-calls",
  "-Xlog-free-types",
  "-Xlog-free-terms",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
) ++ {
  scalaVersion.value match {
    case "2.13.6" => Nil
    case _ => Seq("-Xfatal-warnings")
  }
}

scalacOptions in (Compile,console) ~= {_.filter(_ != "-Ywarn-unused-import")}
scalacOptions in (Test,console) := (scalacOptions in (Compile, console)).value

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "org.scalatest" %% "scalatest" % "3.0.8",
)

developers := List(
  Developer(
    id = "jonaskoelker",
    name = "Jonas Kölker",
    email = "jonaskoelker@yahoo.com",
    url = url("https://github.com/jonaskoelker"),
  )
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/jonaskoelker/equate"),
    "scm:git:git://github.com:jonaskoelker/equate.git",
    "scm:git:ssh://github.com:jonaskoelker/equate.git",
  )
)

val nexus = "https://s01.oss.sonatype.org/"

pomIncludeRepository := { _ => false }
publishTo := {
  if (isSnapshot.value) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  } else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}
publishMavenStyle := true

sonatypeCredentialHost := nexus
