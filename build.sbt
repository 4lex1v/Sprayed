name := "Sprayed"

scalaVersion := "2.10.4"

resolvers += "spray repo" at "http://repo.spray.io/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor"        % "2.3.3",
  "com.typesafe.akka" %% "akka-testkit"      % "2.3.3",
  "io.spray"           % "spray-routing"     % "1.3.1",
  "io.spray"           % "spray-testkit"     % "1.3.1",
  "org.scalaz"        %% "scalaz-core"       % "7.1.0-RC1",
  "org.scalaz"        %% "scalaz-concurrent" % "7.1.0-RC1",
  "org.scalaz"        %% "scalaz-effect"     % "7.1.0-RC1",
  "org.scalatest"     %% "scalatest"         % "2.2.0" % "test"
)