val scala3Version = "3.3.1"

ThisBuild / scalaVersion := scala3Version
ThisBuild / version := "0.1.0"
ThisBuild / scalafmtOnCompile := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc-2023",
    Compile / run / mainClass := Some("aoc2023.day2.run"),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
