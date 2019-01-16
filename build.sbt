
name := "CBRbot"

version := "0.1"

scalaVersion := "2.12.7"

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.5.0",
  "org.typelevel" %% "cats-effect" % "1.1.0",
  "io.chrisdavenport" %% "cats-par" % "0.2.0",
  
  "co.fs2" %% "fs2-core" % "1.0.0",
  "co.fs2" %% "fs2-io" % "1.0.0",
  
  "org.http4s" %% "http4s-blaze-client" % "0.19.0",
  "org.http4s" %% "http4s-circe" % "0.19.0",
  "org.http4s" %% "http4s-dsl" % "0.19.0",
  
  "io.circe" %% "circe-core" % "0.10.1",
  "io.circe" %% "circe-generic" % "0.10.1",

  "io.chrisdavenport" %% "linebacker" % "0.2.0-M2",
  "io.chrisdavenport" %% "log4cats-slf4j" % "0.2.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.pureconfig" %% "pureconfig-cats-effect" % "0.9.2",

  "com.github.cb372" %% "scalacache-core" % "0.27.0",
  "com.github.cb372" %% "scalacache-caffeine" % "0.27.0",
  "com.github.cb372" %% "scalacache-cats-effect" % "0.27.0",


  "net.ruippeixotog" %% "scala-scraper" % "2.1.0",

  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "io.chrisdavenport" %% "log4cats-noop" % "0.2.0" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.8")
addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4")


wartremoverErrors ++= Warts.unsafe
wartremoverErrors -= Wart.Any // false warnings

mainClass in Compile := Some("com.bot.cbr.main.CBRbotApp")


//daemonUser.in(Docker) := "root" // "daemon" by default
maintainer.in(Docker) := "san"
version.in(Docker) := "latest"
//dockerExposedPorts := Vector(9003)
dockerRepository := Some("sanantoha")