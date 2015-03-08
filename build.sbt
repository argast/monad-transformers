name := "monad-transformers"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies := Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.scalaz" %% "scalaz-concurrent" % "7.1.0",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
  "org.typelevel" %% "scalaz-contrib-210" % "0.1.4"
)