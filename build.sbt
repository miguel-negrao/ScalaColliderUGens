name := "scalacolliderugens"

version := "0.14-SNAPSHOT"

organization := "de.sciss"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
   "com.github.scopt" %% "scopt" % "1.1.2",
   "org.scala-lang" % "scala-compiler" % "2.9.1",
   "org.scala-refactoring" % "org.scala-refactoring.library" % "0.3.0-SNAPSHOT" from
        "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.9.1/0.3.0-SNAPSHOT/org.scala-refactoring_2.9.1-0.3.0-20110920.122544-11.jar"
)

retrieveManaged := true