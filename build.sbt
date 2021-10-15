ThisBuild / scalaVersion := "3.0.1"

ThisBuild / scalacOptions += "-feature"
ThisBuild / scalacOptions += "-deprecation"
ThisBuild / scalacOptions += "-unchecked"

ThisBuild / libraryDependencies += "org.apache.poi" % "poi-ooxml" % "4.1.2"
ThisBuild / libraryDependencies += "joda-time" % "joda-time" % "2.10.11"
ThisBuild / libraryDependencies += "commons-io" % "commons-io" % "2.11.0"
