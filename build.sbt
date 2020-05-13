name := "svoboda"
organization := "me.vilunov"

version := "0.1"
scalaVersion := "2.13.2"

scalacOptions ++= Vector(
  "-language:higherKinds",
  "-language:experimental.macros",
  "-Dscala.usejavacp=true",
  "-Ymacro-annotations",
)

libraryDependencies ++= Vector(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided,
  "org.scalatest" %% "scalatest" % "3.1.1" % Test,
)
