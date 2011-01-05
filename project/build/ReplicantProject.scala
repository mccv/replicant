import sbt._

import com.twitter.sbt._

class ReplicantProject(info: ProjectInfo) extends StandardProject(info) with SubversionPublisher {
  override def disableCrossPaths = true
  override def managedStyle = ManagedStyle.Maven

  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "test"
}
