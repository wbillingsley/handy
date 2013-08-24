name := "handy"

organization in ThisBuild := "com.wbillingsley"

version in ThisBuild := "0.4-SNAPSHOT"

scalaVersion in ThisBuild := "2.10.0"

libraryDependencies += "junit" % "junit" % "4.7" % "test"

libraryDependencies += "org.specs2" %% "specs2" % "2.1.1" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test" 

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

crossScalaVersions in ThisBuild := Seq("2.10.0")

publishTo in ThisBuild <<= version { (v: String) =>
  val localm = "/Users/wbillingsley/sourcecode/external/repos/mymavenrepo/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some(Resolver.file("snapshots", new File(localm + "snapshots")))
  else
    Some(Resolver.file("releases", new File(localm + "releases")))
}

parallelExecution in Test := false

