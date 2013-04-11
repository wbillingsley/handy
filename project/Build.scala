import sbt._
import Keys._

object HandyBuild extends Build {

  lazy val root = Project(id = "handy", base = file(".")) 

  lazy val handyplay:Project = Project(id = "handy-play", base = file("handy-play")) dependsOn(root)

}
