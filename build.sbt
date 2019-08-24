// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val commonSettings = Seq(
  organization := "com.wbillingsley",
  version := "0.9.0-SNAPSHOT",
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  crossScalaVersions := Seq("2.11.7", "2.12.8"),
  licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php")),
  homepage := Some(url("http://github.com/wbillingsley/handy")),
  publishMavenStyle := true,
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2-core" % "4.3.4" % "test",
    "junit" % "junit" % "4.7" % "test"
  )
)

// Bintray settings for publishing releases
//seq(bintrayPublishSettings:_*)

publishTo in ThisBuild := Some("Hopper snapshots" at "http://hopper.une.edu.au/artifactory/libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)

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




lazy val handy = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("handy"))
  .settings(commonSettings:_*)
  .settings(
    name := "handy",

    libraryDependencies += "org.reactivestreams" % "reactive-streams" % "1.0.0",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.19" % "test",
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.19" % "test" 
  )

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



lazy val handyappbase = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("handy-appbase-core"))
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
    handyJvm, handyJs, handyplay, handyappbaseJvm, handyappbaseJs, handyuser
  )
  .settings(commonSettings:_*)
  .settings(
    name := "handy-aggregate"
  )
