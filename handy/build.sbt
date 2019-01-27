name := "handy"

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

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.19" % "test"
