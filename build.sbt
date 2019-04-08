scalaVersion := "2.12.7"

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "software.amazon.awssdk" % "ec2" % "2.5.22",
  "org.typelevel" %% "cats-core" % "1.6.0",
  "org.typelevel" %% "cats-free" % "1.6.0",
  "io.monix" %% "monix-reactive" % "3.0.0-RC2",
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

libraryDependencies += "org.typelevel" %% "kittens" % "1.2.0"