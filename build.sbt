import Dependencies._

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Scala Seed Project",
    libraryDependencies ++= Seq(
    "io.spray" %% "spray-json" % "1.3.6",
    "com.lihaoyi" %% "upickle" % "3.1.0",
    "com.lihaoyi" %% "os-lib" % "0.9.1",
    "org.json4s" %% "json4s-native" % "3.6.12",
    munit % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
