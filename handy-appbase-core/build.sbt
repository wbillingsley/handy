libraryDependencies += "com.wbillingsley" %% "salt-encrypt" % "0.1.0-RC1"

libraryDependencies += "com.typesafe.play" %% "play" % "2.2.1"

libraryDependencies += "com.typesafe.play" %% "play-test" % "2.2.1" % "test"

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "bintrayW" at "http://dl.bintray.com/wbillingsley/maven"
