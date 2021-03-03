val scala3Version = "3.0.0-RC1"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .settings(
    name := "stackExchange",
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-rewrite",
      "-indent"
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
