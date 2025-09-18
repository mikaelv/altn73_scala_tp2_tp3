ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "altn73_scala_tp2_tp3",
    idePackagePrefix := Some("tp2_impot")
  )
