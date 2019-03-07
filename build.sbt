organization := "edu.berkeley.cs"

version := "1.0-SNAPSHOT"

name := "testchipip"

scalaVersion := "2.12.4"

scalacOptions += "-Xsource:2.11"

if (sys.props.contains("ROCKET_USE_MAVEN")) {
  libraryDependencies += "edu.berkeley.cs" %% "rocketchip" % "1.2-SNAPSHOT"
} else {
  Seq.empty[Setting[_]] // Top level statements need to be Settings
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := <url>https://github.com/ucb-bar/testchipip</url>
<licenses>
  <license>
    <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/ucb-bar/testchipip.git</url>
    <connection>scm:git:github.com/ucb-bar/testchipip.git</connection>
  </scm>

publishTo := {
  val v = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  }
  else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}
