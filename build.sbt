ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

val scalaVer = "3.6.1"


lazy val root = (project in file("."))
  .settings(
    name := "AoC2024"
  )

libraryDependencies +=  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"