val shared = Seq(
  organization := "edu.berkeley.cs",
  version      := "1.1",
  scalaVersion := "2.12.20",
)

lazy val torture = (project in file("."))
  .aggregate(generator, testrun, overnight, fileop)
  .settings(
    shared,
    name := "torture",
  )

lazy val generator = (project in file("generator"))
  .settings(
    shared,
    name := "generator",
    libraryDependencies ++= Seq(scopt),
  )

lazy val testrun = (project in file("testrun"))
  .dependsOn(generator)
  .settings(
    shared,
    name := "testrun",
    libraryDependencies ++= Seq(scopt),
  )

lazy val overnight = (project in file("overnight"))
  .dependsOn(testrun, fileop)
  .settings(
    shared,
    name := "overnight",
    libraryDependencies ++= Seq(scopt),
  )

lazy val fileop = (project in file("fileop"))
  .settings(
    shared,
    name := "fileop",
    libraryDependencies ++= Seq(scopt),
  )

val scopt  = "com.github.scopt" %% "scopt" % "3.5.0"
