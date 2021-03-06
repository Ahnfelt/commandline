name := "commandline"
organization := "com.github.ahnfelt"
version := "0.4-SNAPSHOT"
scalaVersion := "2.12.4"


publishMavenStyle := true
publishArtifact in Test := false
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if(isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra :=
    <url>https://github.com/Ahnfelt/commandline</url>
        <licenses>
            <license>
                <name>MIT-style</name>
                <url>http://www.opensource.org/licenses/mit-license.php</url>
                <distribution>repo</distribution>
            </license>
        </licenses>
        <scm>
            <url>git@github.com:Ahnfelt/commandline.git</url>
            <connection>scm:git:git@github.com:Ahnfelt/commandline.git</connection>
        </scm>
        <developers>
            <developer>
                <id>ahnfelt</id>
                <name>Joakim Ahnfelt-Rønne</name>
                <url>https://github.com/Ahnfelt</url>
            </developer>
            <developer>
                <id>werk</id>
                <name>Michael Werk Ravnsmed</name>
                <url>https://github.com/werk</url>
            </developer>
        </developers>
