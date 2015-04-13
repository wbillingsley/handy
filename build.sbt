
organization in ThisBuild := "com.wbillingsley"

version in ThisBuild := "0.7.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.6"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

crossScalaVersions in ThisBuild := Seq("2.11.0")

licenses in ThisBuild := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php"))

homepage in ThisBuild := Some(url("http://github.com/wbillingsley/handy"))

publishMavenStyle in ThisBuild := true

// Bintray settings for publishing releases
//seq(bintrayPublishSettings:_*)

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
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


libraryDependencies in ThisBuild ++= Seq(
  "org.specs2" %% "specs2" % "2.3.12" % "test",
  "junit" % "junit" % "4.7" % "test"
)

lazy val handy = (project in file("handy"))

lazy val handyplay = (project in file("handy-play")).dependsOn(handy)

lazy val handyreactivemongo = (project in file("handy-reactivemongo")).dependsOn(handy, handyplay)

lazy val handycasbah = (project in file("handy-casbah")).dependsOn(handy)

lazy val handyuser = (project in file("handy-user")).dependsOn(handy)

lazy val aggregate = (project in file(".")).aggregate(handy, handyplay, handyreactivemongo, handyuser)
