import sbt._
import Keys._

object HandyBuild extends Build {

  lazy val root = Project(id = "handy", base = file(".")) 

  lazy val handyplay:Project = Project(id = "handy-play", base = file("handy-play")) dependsOn(root)

  lazy val handyreactivemongo:Project = Project(id = "handy-reactivemongo", base = file("handy-reactivemongo")) dependsOn(root, handyplay)

  lazy val handyappbasecore:Project = Project(id = "handy-appbase-core", base = file("handy-appbase-core")) dependsOn(root, handyplay, handyreactivemongo)
}
