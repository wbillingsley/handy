

lazy val commonSettings = Seq(
  organization := "com.wbillingsley",
  version := "0.8.0-SNAPSHOT",
  scalaVersion := "2.11.7",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  crossScalaVersions := Seq("2.11.7"),
  licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php")),
  homepage := Some(url("http://github.com/wbillingsley/handy")),
  publishMavenStyle := true,
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.3.12" % "test",
    "junit" % "junit" % "4.7" % "test"
  )
)

// Bintray settings for publishing releases
//seq(bintrayPublishSettings:_*)

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("Hopper snapshots" at "http://hopper.une.edu.au:8081/artifactory/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
    //Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    publishTo.value
}


pomExtra in ThisBuild := (
  <scm>
    <url>git@github.com:wbillingsley/handy.git</url>
    <connection>scm:git:git@github.com:wbillingsley/handy.git</connection>
  </scm>
  <developers>
    <developer>
      <id>wbillingsley</id>
      <name>William Billingsley</name>
      <url>http://www.wbillingsley.com</url>
    </developer>
  </developers>
)




lazy val handy = (crossProject.crossType(CrossType.Pure) in file("handy"))
  .settings(commonSettings:_*)
  .settings(name := "handy")

lazy val handyJvm = handy.jvm
lazy val handyJs = handy.js

lazy val handyplay = (project in file("handy-play"))
  .dependsOn(handyJvm)
  .settings(commonSettings:_*)
  .settings(
    name := "handy-play"
  )

lazy val handyreactivemongo = (project in file("handy-reactivemongo"))
  .dependsOn(handyJvm, handyplay)
  .settings(commonSettings:_*)
  .settings(
    name := "handy-reactivemongo"
  )

lazy val handymongodbasync = (project in file("handy-mongodb-async"))
  .dependsOn(handyJvm, handyplay)
  .settings(commonSettings:_*)
  .settings(
    name := "handy-mongodb-async"
  )



lazy val handyappbase = (crossProject.crossType(CrossType.Pure) in file("handy-appbase-core"))
  .dependsOn(handy)
  .settings(commonSettings:_*)
  .settings(
    name := "handy-appbase"
  )

lazy val handyappbaseJvm = handyappbase.jvm
lazy val handyappbaseJs = handyappbase.js

lazy val handyuser = (project in file("handy-user"))
  .dependsOn(handyJvm, handyappbaseJvm)
  .settings(commonSettings:_*)
  .settings(
    name := "handy-user"
  )


lazy val aggregate = (project in file("."))
  .aggregate(
    handyJvm, handyJs,
    handyplay, handyreactivemongo,
    handyappbaseJvm, handyappbaseJs,
    handyuser,
    handymongodbasync)
  .settings(commonSettings:_*)
  .settings(
    name := "handy-aggregate"
  )
