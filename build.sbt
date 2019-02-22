name := "nfs-simu"

version := "0.6.2"

scalaVersion := "2.10.7"

scalacOptions ++= Seq("-deprecation")

libraryDependencies <+= scalaVersion { sv =>
  "org.scala-lang" % "scala-swing" % sv
}

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

libraryDependencies += "org.swinglabs" % "swingx" % "1.6" % "compile" withSources()

libraryDependencies += "com.github.benhutchison" % "scalaswingcontrib" % "1.5"

// libraryDependencies += "commons-codec" % "commons-codec" % "1.7"
//
libraryDependencies += "io.spray" %% "spray-json" % "1.2.6"

libraryDependencies += "commons-lang" % "commons-lang" % "2.2"

libraryDependencies += "commons-codec" % "commons-codec" % "1.9"

libraryDependencies += "commons-logging" % "commons-logging" % "1.1.3"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-annotations" % "2.4.2"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.4.2"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.2"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

retrieveManaged := true

initialCommands in console := """
"""

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in oneJar := Some("com.allinfinance.nfs.simu.NfsSimu")


