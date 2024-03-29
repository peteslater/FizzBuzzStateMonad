name := "FizzBuzz"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

scalacOptions += "-Ypartial-unification"