organization  := "feh.dsl"

name := "graphviz"

version := "0.2-SNAPSHOT"

crossScalaVersions := Seq("2.11.5", "2.10.4")

scalaVersion := crossScalaVersions.value.head

resolvers += "Fehu's github repo" at "http://fehu.github.io/repo"

libraryDependencies += "feh.util" %% "util" % "1.0.8-SNAPSHOT"
