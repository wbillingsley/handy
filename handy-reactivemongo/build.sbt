resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.reactivemongo" %% "reactivemongo" % "0.10.5.0.akka23"
