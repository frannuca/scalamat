import sbt._
import sbt.Keys._


object BuildSettings {


  val buildOrganization = "org.fjn"
  val buildVersion      = "1.0.0"
  val buildScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion//,
  )
}

object Resolvers {
     val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

}

object Dependencies {
     val actors =  "com.typesafe.akka" %% "akka-actor" % "2.2.3"
  val scalaSwing = "org.scala-lang" %  "scala-swing"  % BuildSettings.buildScalaVersion
  val apacheMath =  "org.apache.commons" % "commons-math3" % "3.0"
  val jFree = "jfree" % "jfreechart" % "1.0.13"

}

object ScalaMatBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  /**
   * top layer
   */
  lazy val scalaMatPrj = Project (
    "scalamat",
    file ("."),
    settings = buildSettings++ Seq (resolvers :=  Seq(typesafe), libraryDependencies ++=Seq(actors,scalaSwing,apacheMath,jFree))

  ) //aggregate (optimizer,ia, org.fjn.org.fjn.org.fjn.pythia.pricers)



}
