scalaVersion := "2.11.7"

libraryDependencies <++= (scalaVersion)(ver =>
  Seq(
    "org.scala-lang" % "scala-reflect"  % ver,
    "org.scala-lang" % "scala-compiler" % ver
  )
)

scalacOptions ++= Seq("-feature", "-deprecation")

initialCommands in console := """
import exteff._
import exteff.Union._
"""
