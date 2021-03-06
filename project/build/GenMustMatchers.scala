import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source

object Helper {
  def translateShouldToMust(shouldLine: String): String = {
    val temp1 = shouldLine.replaceAll("<code>must</code>", "<code>I_WAS_must_ORIGINALLY</code>")
    val temp2 = temp1.replaceAll("<!-- PRESERVE -->should", " I_MUST_STAY_SHOULD")
    val temp3 = temp2.replaceAll(
      "<a href=\"MustMatchers.html\"><code>MustMatchers</code></a>",
      "<a href=\"I_WAS_Must_ORIGINALLYMatchers.html\"><code>I_WAS_Must_ORIGINALLYMatchers</code></a>"
    )
    val temp4 = temp3.replaceAll("should", "must")
    val temp5 = temp4.replaceAll("Should", "Must")
    val temp6 = temp5.replaceAll("I_WAS_must_ORIGINALLY", "should")
    val temp7 = temp6.replaceAll("I_MUST_STAY_SHOULD", "should")
    temp7.replaceAll("I_WAS_Must_ORIGINALLY", "Should")
  }
  def generateFile(srcFileName: String, targetFileName: String) {
    val dir = new File("main/must_matchers/src/main/scala/org/scalatest/matchers")
    dir.mkdirs()
    val filename = "main/must_matchers/src/main/scala/org/scalatest/matchers/" + targetFileName
    println("generating: " + filename)
    val writer = new BufferedWriter(new FileWriter(filename))
    try {
      val shouldLines = Source.fromFile("main/src/main/scala/org/scalatest/matchers/" + srcFileName).getLines.toList
      for (shouldLine <- shouldLines) {
        val mustLine = translateShouldToMust(shouldLine)
        writer.write(mustLine)
      }
    }
    finally {
      writer.close()
    }
  }
}

import Helper._

object GenMustMatchers {
  def generate() = generateFile("ShouldMatchers.scala", "MustMatchers.scala")
}

object GenMustMatchersTests {

  def generate() {
    val dir = new File("main/must_matchers/src/test/scala/org/scalatest/matchers")
    dir.mkdirs()
    val shouldFileNames =
    List(
        "ShouldBehaveLikeSpec.scala",
        "ShouldContainElementSpec.scala",
        "ShouldContainKeySpec.scala",
        "ShouldContainValueSpec.scala",
        "ShouldEqualSpec.scala",
        "ShouldHavePropertiesSpec.scala",
        "ShouldLengthSpec.scala",
        "ShouldOrderedSpec.scala",
        "ShouldSizeSpec.scala",
        // "ShouldStackSpec.scala", now in examples
        // "ShouldStackFlatSpec.scala",
        "ShouldBeASymbolSpec.scala",
        "ShouldBeAnSymbolSpec.scala",
        "ShouldBeMatcherSpec.scala",
        "ShouldBePropertyMatcherSpec.scala",
        "ShouldBeSymbolSpec.scala",
        "ShouldEndWithRegexSpec.scala",
        "ShouldEndWithSubstringSpec.scala",
        "ShouldFullyMatchSpec.scala",
        "ShouldIncludeRegexSpec.scala",
        "ShouldIncludeSubstringSpec.scala",
        "ShouldLogicalMatcherExprSpec.scala",
        "ShouldMatcherSpec.scala",
        "ShouldPlusOrMinusSpec.scala",
        "ShouldSameInstanceAsSpec.scala",
        "ShouldStartWithRegexSpec.scala",
        "ShouldStartWithSubstringSpec.scala",
        "ShouldBeNullSpec.scala"
        )

    for (shouldFileName <- shouldFileNames) {

      val mustFileName = shouldFileName.replace("Should", "Must")
      val filename = "main/must_matchers/src/test/scala/org/scalatest/matchers/" + mustFileName
      println("generating: " + filename)
      val writer = new BufferedWriter(new FileWriter(filename))
      try {
        val shouldLines = Source.fromFile("main/src/test/scala/org/scalatest/matchers/" + shouldFileName).getLines.toList
        for (shouldLine <- shouldLines) {
          val mustLine = translateShouldToMust(shouldLine)
          writer.write(mustLine.toString)
        }
      }
      finally {
        writer.close()
      }
    }
  }
}
