import sbt._

class ScalaColliderUGensProject( info: ProjectInfo ) extends DefaultProject( info ) {
//   val refactoring      = "org.scala-refactoring" % "org.scala-refactoring.library" % "0.2.0-SNAPSHOT"
//   val scalaToolsSnaps  = "Scala-Tools Snapshots" at "http://scala-tools.org/repo-snapshots"

   val scalaRefactoring = "org.scala-refactoring" % "org.scala-refactoring.library" % "0.3.0-SNAPSHOT" from
//      "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring.library/0.3.0-SNAPSHOT/org.scala-refactoring.library-0.3.0-20110106.074115-13.jar"
      "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring.library/0.3.0-SNAPSHOT/org.scala-refactoring.library-0.3.0-20110227.125851-28.jar"
}