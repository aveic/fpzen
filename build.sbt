name := "untitled"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions"     // Allow definition of implicit functions called views
)


libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.typesafe.akka" %% "akka-actor" % "2.5.3"
)

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql" % "9.3-1102-jdbc41",
  "org.scalikejdbc" %% "scalikejdbc"               % "2.5.2",
  "ch.qos.logback"  %  "logback-classic"           % "1.2.3"
)

//addCompilerPlugin("org.scalameta" % "paradise" % "3.3.1" cross CrossVersion.full)
