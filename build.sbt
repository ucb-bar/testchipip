ThisBuild / organization := "edu.berkeley.cs"
ThisBuild / version := "1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.10"

lazy val sifiveblocks = RootProject(uri("https://github.com/sifive/sifive-blocks.git"))

lazy val root = (project in file "."))
    .dependsOn(sifiveblocks)
    .settings(
      name := "testchipip",
	  scalacOptions += "-Xsource:2.11",
      libraryDependencies += "edu.berkeley.cs" %% "rocketchip" % "1.5-SNAPSHOT",

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
      },

      resolvers ++= Seq(
        Resolver.sonatypeRepo("snapshots"),
        Resolver.sonatypeRepo("releases"),
        Resolver.mavenLocal),
    )



