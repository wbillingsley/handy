name := "handy"

organization in ThisBuild := "com.wbillingsley"

version in ThisBuild := "0.5.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.10.3"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

crossScalaVersions in ThisBuild := Seq("2.10.3")

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

parallelExecution in Test := false

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


libraryDependencies += "junit" % "junit" % "4.7" % "test"

libraryDependencies += "org.specs2" %% "specs2" % "2.1.1" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test" 
