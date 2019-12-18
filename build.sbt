name := "MyProject"
version := "1.2.8"
scalaVersion := "2.13.1"

val akkaVersion = "2.5.22"

libraryDependencies ++= 
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion
  )
