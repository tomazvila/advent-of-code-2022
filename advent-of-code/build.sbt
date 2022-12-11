val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.14" % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.14.0" % "test"
    
  )
