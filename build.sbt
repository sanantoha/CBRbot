import sbt.Keys.scalacOptions

name := "CBRbot"

version := "0.1"

scalaVersion := "2.13.4"
//crossScalaVersions := Seq("2.12.12", "2.13.4")

//enablePlugins(JavaAppPackaging)
//enablePlugins(DockerPlugin)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  
  "org.typelevel" %% "cats-core" % "2.3.1",
  "org.typelevel" %% "cats-effect" % "2.3.1",
  
  "co.fs2" %% "fs2-core" % "2.4.6",
  "co.fs2" %% "fs2-io" % "2.4.6",
  
  "org.http4s" %% "http4s-blaze-client" % "1.0-107-6676c1e",
  "org.http4s" %% "http4s-circe" % "1.0-107-6676c1e",
  "org.http4s" %% "http4s-dsl" % "1.0-107-6676c1e",
  
  "io.circe" %% "circe-core" % "0.13.0",
  "io.circe" %% "circe-generic" % "0.13.0",

  "org.tpolecat" %% "doobie-core" % "0.9.2",
  "io.chrisdavenport" %% "log4cats-slf4j" % "1.1.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.pureconfig" %% "pureconfig" % "0.14.0",
  "com.github.pureconfig" %% "pureconfig-cats" % "0.14.0",
  "com.github.pureconfig" %% "pureconfig-cats-effect" % "0.14.0",

  "com.github.cb372" %% "scalacache-core" % "0.28.0",
  "com.github.cb372" %% "scalacache-caffeine" % "0.28.0",
  "com.github.cb372" %% "scalacache-cats-effect" % "0.28.0",


  "net.ruippeixotog" %% "scala-scraper" % "2.2.0",

  "org.scalatest" %% "scalatest" % "3.2.3" % "test",
  "io.chrisdavenport" %% "log4cats-noop" % "1.1.1" % "test"
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.2" cross CrossVersion.full)
addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.1")

scalacOptions ++= List("-Ymacro-annotations", "-Wconf:any:wv")

//wartremoverErrors ++= Warts.unsafe
//wartremoverErrors -= Wart.Any // false warnings

//mainClass in Compile := Some("com.bot.cbr.main.CBRbotApp")
//
//
////daemonUser.in(Docker) := "root" // "daemon" by default
//maintainer.in(Docker) := "san"
//version.in(Docker) := "latest"
////dockerExposedPorts := Vector(9003)
//dockerRepository := Some("sanantoha")