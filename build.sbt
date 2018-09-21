///////////////////////////////////////////////////////////////////////////////////////////////////
// Settings
///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val buildSettings = Seq(
  organization := "io.pjan",
  scalaVersion := "2.12.6"
)

lazy val commonSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  libraryDependencies ++= Seq(
    compilerPlugin(D.betterMonadicFor)
  ),
  scalacOptions ++= commonScalacOptions,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value
    .filter(_ != "-Xfatal-warnings"),
  scalacOptions in (Compile, console) ~= {
    _.filterNot(Seq("-Xlint", "-Ywarn-unused-import").contains)
  },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  git.useGitDescribe := true
)

lazy val testSettings = Seq(
  fork in Test := true,
  parallelExecution in Test := false
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Xlog-reflective-calls",
  "-Ywarn-inaccessible",
  "-Ypatmat-exhaust-depth",
  "20",
  "-Ydelambdafy:method",
  "-Xmax-classfile-name",
  "100",
  "-Ypartial-unification",
  "-Ywarn-unused-import"
)

lazy val commonJvmSettings = Seq(
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val resolverSettings = Seq(
  resolvers ++= Seq("Typesafe Releases" at "http://repo.typesafe.com/typesafe/maven-releases/")
)

lazy val formatSettings = Seq(
  scalafmtOnCompile := true
)

lazy val projectSettings = buildSettings ++ commonSettings ++ commonJvmSettings ++ formatSettings ++ resolverSettings

///////////////////////////////////////////////////////////////////////////////////////////////////
// Dependencies
///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val D = new {

  lazy val Versions = new {
    val cats        = "1.3.1"
    val catsEffect  = "1.0.0"

    // Test

    val catsScalaCheck    = "0.1.0"
    val scalaCheck        = "1.14.0"
    val scalaTest         = "3.0.5"

    // Compiler
    val betterMonadicFor = "0.2.4"

  }

  lazy val cats        = "org.typelevel"              %% "cats-core"      % Versions.cats
  lazy val catsEffect  = "org.typelevel"              %% "cats-effect"    % Versions.catsEffect

  // Compiler plugins
  lazy val betterMonadicFor = "com.olegpy"     %% "better-monadic-for" % Versions.betterMonadicFor

  // Test
  lazy val scalaTest      = "org.scalatest"     %% "scalatest"        % Versions.scalaTest
  lazy val scalaCheck     = "org.scalacheck"    %% "scalacheck"       % Versions.scalaCheck
  lazy val catsScalaCheck = "io.chrisdavenport" %% "cats-scalacheck"  % Versions.catsScalaCheck
  lazy val catsEffectLaws = "org.typelevel"     %% "cats-effect-laws" % Versions.catsEffect

}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Projects
///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val scio = Project(
  id = "root",
  base = file(".")
).settings(
    libraryDependencies ++= Seq(
      D.cats,
      D.catsEffect,
      D.scalaTest % "it,test",
      D.catsEffectLaws % "it,test"
    )
  )
  .settings(projectSettings)
  .settings(testSettings)
