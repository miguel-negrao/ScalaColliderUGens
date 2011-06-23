import sbt._

class ScalaColliderUGensProject( info: ProjectInfo ) extends DefaultProject( info ) {
//   val refactoring      = "org.scala-refactoring" % "org.scala-refactoring.library" % "0.2.0-SNAPSHOT"
//   val scalaToolsSnaps  = "Scala-Tools Snapshots" at "http://scala-tools.org/repo-snapshots"

   val scalaRefactoring = "org.scala-refactoring" % "org.scala-refactoring.library" % "0.3.0-SNAPSHOT" from
//      "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring.library/0.3.0-SNAPSHOT/org.scala-refactoring.library-0.3.0-20110106.074115-13.jar"
//      "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring.library/0.3.0-SNAPSHOT/org.scala-refactoring.library-0.3.0-20110227.125851-28.jar"
      "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.9.0/0.3.0-SNAPSHOT/org.scala-refactoring_2.9.0-0.3.0-20110623.135229-19.jar"
//   "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.8.1/0.3.0-SNAPSHOT/org.scala-refactoring_2.8.1-0.3.0-20110602.085025-16.jar"
//   "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring.library/0.3.0-SNAPSHOT/org.scala-refactoring.library-0.3.0-20110316.193756-32.jar"

//   val scala = "org.scala-lang" % "scala-compiler" % "2.8.1"

    override def compileOptions = super.compileOptions ++ Seq(Unchecked)
}