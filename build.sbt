import Dependencies.*

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version      := "0.1.0-SNAPSHOT"

Compile / compile / scalacOptions ++= List(
  "-deprecation",
  "-encoding",
  "utf-8",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Ywarn-dead-code",
  "-Ywarn-extra-implicit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
  "-Ywarn-value-discard",
  "-Ywarn-unused:privates"
)

addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full)
addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")

lazy val task1 = (project in file("task1"))
  .settings(
    name := "task1",
    libraryDependencies ++= List(
      catsCore,
      scalaTest % Test
    ),
    coverageEnabled                 := true,
    coverageFailOnMinimum           := true,
    coverageMinimumStmtTotal        := 70,
    coverageMinimumBranchTotal      := 70,
    coverageMinimumStmtPerPackage   := 70,
    coverageMinimumBranchPerPackage := 65,
    coverageMinimumStmtPerFile      := 65,
    coverageMinimumBranchPerFile    := 60
  )

lazy val root = (project in file("."))
  .settings(name := "scalapersonal")
  .aggregate(task1)
