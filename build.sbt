name := "swissborg-challenge"

version := "0.1"

scalaVersion := "2.13.8"

val http4sVersion = "1.0.0-M32"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.3.11"
)

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-ember-client" % http4sVersion
)

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "io.circe" %% "circe-generic" % "0.14.1",
  "io.circe" %% "circe-literal" % "0.14.1"
)

libraryDependencies += "io.circe" %% "circe-parser" % "0.14.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test
