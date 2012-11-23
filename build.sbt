name := "handy"

organization := "com.wbillingsley"

version := "0.2-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies += "junit" % "junit" % "4.7" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test" 

scalacOptions ++= Seq("-unchecked", "-deprecation")

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0-RC2")

