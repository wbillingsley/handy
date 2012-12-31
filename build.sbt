name := "handy"

organization := "com.wbillingsley"

version := "0.3-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies += "junit" % "junit" % "4.7" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test" 

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0")

publishTo <<= version { (v: String) =>
  val localm = "/Users/wbillingsley/sourcecode/external/repos/mymavenrepo/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some(Resolver.file("snapshots", new File(localm + "snapshots")))
  else
    Some(Resolver.file("releases", new File(localm + "releases")))
}

