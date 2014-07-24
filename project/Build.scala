import sbt._
import Keys._

object HandyBuild extends Build {

  lazy val root = Project(id = "handy", base = file(".")) 

  lazy val handyplay:Project = Project(id = "handy-play", base = file("handy-play")) dependsOn(root)

  lazy val handyreactivemongo:Project = Project(id = "handy-reactivemongo", base = file("handy-reactivemongo")) dependsOn(root, handyplay)

  lazy val handycasbah:Project = Project(id = "handy-casbah", base = file("handy-casbah")) dependsOn(root)

  lazy val handyuser:Project = Project(id = "handy-user", base = file("handy-user")) dependsOn(root)

  lazy val aggregate:Project = Project(id = "handy-agg-all", base = file("handy-agg-all")) aggregate(root, handyplay, handyreactivemongo, handyuser)

}
