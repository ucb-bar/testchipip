organization := "edu.berkeley.cs"

version := "1.0-SNAPSHOT"

name := "testchipip"

scalaVersion := "2.13.16"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal,
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/service/local/repositories/snapshots/content"
)

lazy val rocketchip_blocks = RootProject(uri("https://github.com/ucb-bar/rocket-chip-blocks.git#883fe82a3e31bdc22e63050553de36ff02e5e5f0"))

lazy val root = (project in file("."))
  .dependsOn(rocketchip_blocks)
  .settings(
    libraryDependencies += "edu.berkeley.cs" %% "rocketchip-6.0.0" % "1.6-6.0.0-1b9f43352-SNAPSHOT",
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
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
      </scm>,
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
)
