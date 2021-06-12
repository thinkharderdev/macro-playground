val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .dependsOn(app,macros)
  .settings(
    name := "macro-playground",
    version := "0.1.0",
    scalaVersion := scala3Version
  )

lazy val app = project
  .in(file("app"))
  .dependsOn(macros)
  .settings(
    name := "macro-playground",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test-magnolia" % "1.0.9"
    )
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "macro-playground",
    version := "0.1.0",
    scalaVersion := scala3Version
  )
