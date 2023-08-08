val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "LangPlayground",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    // scalacOptions ++= Seq(
    //   "-Xprint:postInlining"
    // ),
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

addCommandAlias(
  "testCoverage",
  "clean ; coverage ; test ; coverageReport"
)
