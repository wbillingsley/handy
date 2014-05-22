libraryDependencies += "com.typesafe.play" %% "play" % "2.3.0-RC1"

libraryDependencies += "com.typesafe.play" %% "play-test" % "2.3.0-RC1" % "test"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "bintrayW" at "http://dl.bintray.com/wbillingsley/maven"
