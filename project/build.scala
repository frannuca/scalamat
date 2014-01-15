import sbt._
import sbt.Keys._


object BuildSettings {


  val buildOrganization = "org.fjn"
  val buildVersion = "1.0.0"
  val buildScalaVersion = "2.10.3"
  val scalazVersion = "7.0.5"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion //,
  )
}

object Resolvers {

  val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  val sonatype1 = "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
  val sonatype2 = "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
}

object Dependencies {


  val actors = "com.typesafe.akka" %% "akka-actor" % "2.2.3"
  val scalaSwing = "org.scala-lang" % "scala-swing" % BuildSettings.buildScalaVersion
  val apacheMath = "org.apache.commons" % "commons-math3" % "3.0"
  val jFree = "jfree" % "jfreechart" % "1.0.13"
  val shapeless = "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full
  val specs2 = "org.specs2" %% "specs2" % "2.3.6" % "test"

  val scalazCore = "org.scalaz" %% "scalaz-core" % BuildSettings.scalazVersion
  val scalazEffect = "org.scalaz" %% "scalaz-effect" % BuildSettings.scalazVersion
  val scalaztypeLevel = "org.scalaz" %% "scalaz-typelevel" % BuildSettings.scalazVersion
  val scalazBinding = "org.scalaz" %% "scalaz-scalacheck-binding" % BuildSettings.scalazVersion % "test"

}

object ScalaMatBuild extends Build {

  import Resolvers._
  import Dependencies._
  import BuildSettings._

  /**
   * top layer
   */
  lazy val scalaMatPrj = Project(
    "scalamat",
    file("."),
    settings = buildSettings ++ Seq(resolvers := Seq(typesafe, sonatype1, sonatype2), libraryDependencies ++=
      Seq(specs2, actors, scalaSwing, apacheMath, jFree, shapeless, scalazCore, scalazEffect, scalaztypeLevel, scalazBinding))
  ) //aggregate (optimizer,ia, org.fjn.org.fjn.org.fjn.pythia.pricers)


}
