import sbt._

class ScalaTestProject(info: ProjectInfo) extends ParentProject(info) {

  lazy val main = project("main", "ScalaTest", new Main(_))
  lazy val examples = project("examples", "examples", new Examples(_), main)
  lazy val must = project("main" / "must_matchers", "must_matchers", new MustMatchers(_), main)

  class ScalaTestParentProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
    override def crossScalaVersions = Set("2.7.5")
    // puts sbt on the classpath
    override def compileClasspath = super.compileClasspath +++ Path.fromFile(FileUtilities.sbtJar)
  }

  class Main(info: ProjectInfo) extends ScalaTestParentProject(info){
    override def packagePaths = super.packagePaths +++ must.packagePaths

    // dependencies
    val junit = "junit" % "junit" % "4.4"
    val testng = "org.testng" % "testng" % "5.7" from
                 "http://repo1.maven.org/maven2/org/testng/testng/5.7/testng-5.7-jdk15.jar"
    val ant = "org.apache.ant" % "ant" % "1.7.1"
    val hamcrest = "org.hamcrest" % "hamcrest-all" % "1.1"
    val scalacheck = "org.scala-tools.testing" % "scalacheck" % "1.5"

    val mockito = "org.mockito" % "mockito-all" % "1.7" % "test->default"
    val jmock = "org.jmock" % "jmock" % "2.4.0" % "test->default"

   /** cobertura.jar   code coverage ... **/

    // which tests to run
    override def includeTest( name: String ) = {
      def exclude(names:String*): Boolean = ! names.find(name startsWith _ ).isDefined

      exclude("org.scalatest.testng.example",
             "org.scalatest.testng.testpackage",
             "org.scalatest.tools",
             "org.scalatest.junit.helpers") &&
      name.startsWith("org.scalatest.PackageAccess")
    }
  }

  class Examples(info: ProjectInfo) extends ScalaTestParentProject(info){
    override def artifactID = "scalatest-examples"
  }

  class MustMatchers(info: ProjectInfo) extends ScalaTestParentProject(info) {
    lazy val generateSrc = task { GenMustMatchers.generate(); None }
    override def compileAction = super.compileAction dependsOn(generateSrc)

    lazy val generateTestSrc = task { GenMustMatchersTests.generate(); None }
    override def testCompileAction = super.testCompileAction dependsOn(generateTestSrc)
  }
}

// which packages to run and which not to run
//override def includeTest( name: String ) = ! name.startsWith("org.scalatest.tools")

//override def mainSourceRoots = super.mainSourceRoots +++ ("target" / "generated" / "main" / "scala")
//override def testSourceRoots = super.testSourceRoots +++ ("target" / "generated" / "test" / "scala")
//override def testSourceRoots = super.testSourceRoots +++ (testSourcePath / "generated" / "scala")
