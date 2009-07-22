/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.matchers

import prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._

class MustIncludeSubstringSpec extends Spec with MustMatchers with Checkers with ReturnsNormallyThrowsAssertion {

describe("The include substring syntax") {

    it("must do nothing if the string includes the specified substring") {

      "1.78" must include ("1.7")
      "21.7" must include ("1.7")
      "21.78" must include ("1.7")
      "1.7" must include ("1.7")
      check((s: String, t: String, u: String) => returnsNormally(s + t + u must include (t)))
    }

    it("must do nothing if the string does not include the specified substring when used with not") {

      "eight" must not { include ("1.7") }
      "eight" must not include ("1.7")
      check((s: String, t: String, u: String) => (s + u).indexOf(t) == -1 ==> returnsNormally(s + u must not (include (t))))
      check((s: String, t: String, u: String) => (s + u).indexOf(t) == -1 ==> returnsNormally(s + u must not include (t)))
    }

    it("must do nothing if the string does not include the specified substring when used in a logical-and expression") {

      "a1.7" must (include ("1.7") and (include ("1.7")))
      "a1.7" must (include ("1.7") and (include ("1.7")))
      "a1.7" must (include ("1.7") and (include ("1.7")))

      "1.7b" must ((include ("1.7")) and (include ("1.7")))
      "1.7b" must ((include ("1.7")) and (include ("1.7")))
      "1.7b" must ((include ("1.7")) and (include ("1.7")))

      "a1.7b" must (include ("1.7") and include ("1.7"))
      "a1.7b" must (include ("1.7") and include ("1.7"))
      "a1.7b" must (include ("1.7") and include ("1.7"))

      "1.7" must (include ("1.7") and (include ("1.7")))
      "1.7" must ((include ("1.7")) and (include ("1.7")))
      "1.7" must (include ("1.7") and include ("1.7"))

      check((s: String, t: String, u: String) => returnsNormally(s + t + u must (include (s) and include (t) and include (u))))
    }

    it("must do nothing if the string does not include the specified substring when used in a logical-or expression") {

      "a1.7" must (include ("hello") or (include ("1.7")))
      "a1.7" must (include ("hello") or (include ("1.7")))
      "a1.7" must (include ("hello") or (include ("1.7")))

      "1.7b" must ((include ("hello")) or (include ("1.7")))
      "1.7b" must ((include ("hello")) or (include ("1.7")))
      "1.7b" must ((include ("hello")) or (include ("1.7")))

      "a1.7b" must (include ("hello") or include ("1.7"))
      "a1.7b" must (include ("hello") or include ("1.7"))
      "a1.7b" must (include ("hello") or include ("1.7"))

      "1.7" must (include ("hello") or (include ("1.7")))
      "1.7" must ((include ("hello")) or (include ("1.7")))
      "1.7" must (include ("hello") or include ("1.7"))

      check((s: String, t: String, u: String) => returnsNormally(s + t + u must (include ("hi") or include ("ho") or include (t))))
    }

    it("must do nothing if the string does not include the specified substring when used in a logical-and expression with not") {
      "fred" must (not (include ("bob")) and not (include ("1.7")))
      "fred" must ((not include ("bob")) and (not include ("1.7")))
      "fred" must (not include ("bob") and not include ("1.7"))
      check((s: String) => s.indexOf("bob") == -1 && s.indexOf("1.7") == -1 ==> returnsNormally(s must (not include ("bob") and not include ("1.7"))))
    }

    it("must do nothing if the string does not include the specified substring when used in a logical-or expression with not") {
      "fred" must (not (include ("fred")) or not (include ("1.7")))
      "fred" must ((not include ("fred")) or (not include ("1.7")))
      "fred" must (not include ("fred") or not include ("1.7"))
      check((s: String) => s.indexOf("a") == -1 || s.indexOf("b") == -1 ==> returnsNormally(s must (not include ("a") or not include ("b"))))
    }

    it("must throw TestFailedException if the string does not match the specified substring") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must include ("1.78")
      }
      assert(caught1.getMessage === "\"1.7\" did not include substring \"1.78\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must include ("21.7")
      }
      assert(caught2.getMessage === "\"1.7\" did not include substring \"21.7\"")

      val caught3 = intercept[TestFailedException] {
        "-one.eight" must include ("1.7")
      }
      assert(caught3.getMessage === "\"-one.eight\" did not include substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "eight" must include ("1.7")
      }
      assert(caught6.getMessage === "\"eight\" did not include substring \"1.7\"")

      val caught7 = intercept[TestFailedException] {
        "one.eight" must include ("1.7")
      }
      assert(caught7.getMessage === "\"one.eight\" did not include substring \"1.7\"")

      val caught8 = intercept[TestFailedException] {
        "onedoteight" must include ("1.7")
      }
      assert(caught8.getMessage === "\"onedoteight\" did not include substring \"1.7\"")

      val caught9 = intercept[TestFailedException] {
        "***" must include ("1.7")
      }
      assert(caught9.getMessage === "\"***\" did not include substring \"1.7\"")

      check((s: String) => s.indexOf("1.7") == -1 ==> throwsTestFailedException(s must include ("1.7")))
    }

    it("must throw TestFailedException if the string does matches the specified substring when used with not") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must not { include ("1.7") }
      }
      assert(caught1.getMessage === "\"1.7\" included substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must not { include ("1.7") }
      }
      assert(caught2.getMessage === "\"1.7\" included substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "-1.8" must not { include ("1.8") }
      }
      assert(caught3.getMessage === "\"-1.8\" included substring \"1.8\"")

      val caught4 = intercept[TestFailedException] {
        "8" must not { include ("8") }
      }
      assert(caught4.getMessage === "\"8\" included substring \"8\"")

      val caught5 = intercept[TestFailedException] {
        "1." must not { include (".") }
      }
      assert(caught5.getMessage === "\"1.\" included substring \".\"")

      val caught11 = intercept[TestFailedException] {
        "1.7" must not include ("1.7")
      }
      assert(caught11.getMessage === "\"1.7\" included substring \"1.7\"")

      val caught13 = intercept[TestFailedException] {
        "-1.8" must not include ("-")
      }
      assert(caught13.getMessage === "\"-1.8\" included substring \"-\"")

      val caught14 = intercept[TestFailedException] {
        "8" must not include ("")
      }
      assert(caught14.getMessage === "\"8\" included substring \"\"")

      val caught15 = intercept[TestFailedException] {
        "1." must not include ("1.")
      }
      assert(caught15.getMessage === "\"1.\" included substring \"1.\"")

      val caught21 = intercept[TestFailedException] {
        "a1.7" must not { include ("1.7") }
      }
      assert(caught21.getMessage === "\"a1.7\" included substring \"1.7\"")

      val caught22 = intercept[TestFailedException] {
        "1.7b" must not { include ("1.7") }
      }
      assert(caught22.getMessage === "\"1.7b\" included substring \"1.7\"")

      val caught23 = intercept[TestFailedException] {
        "a-1.8b" must not { include ("1.8") }
      }
      assert(caught23.getMessage === "\"a-1.8b\" included substring \"1.8\"")

      // substring at the beginning
      check((s: String) => s.length != 0 ==> throwsTestFailedException(s must not include (s.substring(0, 1))))
      // substring at the end
      check((s: String) => s.length != 0 ==> throwsTestFailedException(s must not include (s.substring(s.length - 1, s.length))))
      // substring in the middle
      check((s: String) => s.length > 1 ==> throwsTestFailedException(s must not include (s.substring(1, 2))))
    }

    it("must throw TestFailedException if the string includes the specified substring when used in a logical-and expression") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must (include ("1.7") and (include ("1.8")))
      }
      assert(caught1.getMessage === "\"1.7\" included substring \"1.7\", but \"1.7\" did not include substring \"1.8\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must ((include ("1.7")) and (include ("1.8")))
      }
      assert(caught2.getMessage === "\"1.7\" included substring \"1.7\", but \"1.7\" did not include substring \"1.8\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" must (include ("1.7") and include ("1.8"))
      }
      assert(caught3.getMessage === "\"1.7\" included substring \"1.7\", but \"1.7\" did not include substring \"1.8\"")

      // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
      val caught4 = intercept[TestFailedException] {
        "one.eight" must (include ("1.7") and (include ("1.8")))
      }
      assert(caught4.getMessage === "\"one.eight\" did not include substring \"1.7\"")

      val caught5 = intercept[TestFailedException] {
        "one.eight" must ((include ("1.7")) and (include ("1.8")))
      }
      assert(caught5.getMessage === "\"one.eight\" did not include substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "one.eight" must (include ("1.7") and include ("1.8"))
      }
      assert(caught6.getMessage === "\"one.eight\" did not include substring \"1.7\"")

      check((s: String, t: String, u: String) => (s + u).indexOf(t) == -1 ==> throwsTestFailedException(s + u must (include (s) and include (t))))
    }

    it("must throw TestFailedException if the string includes the specified substring when used in a logical-or expression") {

      val caught1 = intercept[TestFailedException] {
        "one.seven" must (include ("1.7") or (include ("1.8")))
      }
      assert(caught1.getMessage === "\"one.seven\" did not include substring \"1.7\", and \"one.seven\" did not include substring \"1.8\"")

      val caught2 = intercept[TestFailedException] {
        "one.seven" must ((include ("1.7")) or (include ("1.8")))
      }
      assert(caught2.getMessage === "\"one.seven\" did not include substring \"1.7\", and \"one.seven\" did not include substring \"1.8\"")

      val caught3 = intercept[TestFailedException] {
        "one.seven" must (include ("1.7") or include ("1.8"))
      }
      assert(caught3.getMessage === "\"one.seven\" did not include substring \"1.7\", and \"one.seven\" did not include substring \"1.8\"")

      check(
        (s: String, t: String, u: String, v: String) => {
          (t.length != 0 && v.length != 0 && (s + u).indexOf(t) == -1 && (s + u).indexOf(v) == -1) ==>
            throwsTestFailedException(s + u must (include (t) or include (v)))
        }
      )
    }

    it("must throw TestFailedException if the string includes the specified substring when used in a logical-and expression used with not") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must (not include ("1.8") and (not include ("1.7")))
      }
      assert(caught1.getMessage === "\"1.7\" did not include substring \"1.8\", but \"1.7\" included substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must ((not include ("1.8")) and (not include ("1.7")))
      }
      assert(caught2.getMessage === "\"1.7\" did not include substring \"1.8\", but \"1.7\" included substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" must (not include ("1.8") and not include ("1.7"))
      }
      assert(caught3.getMessage === "\"1.7\" did not include substring \"1.8\", but \"1.7\" included substring \"1.7\"")

      val caught4 = intercept[TestFailedException] {
        "a1.7" must (not include ("1.8") and (not include ("1.7")))
      }
      assert(caught4.getMessage === "\"a1.7\" did not include substring \"1.8\", but \"a1.7\" included substring \"1.7\"")

      val caught5 = intercept[TestFailedException] {
        "1.7b" must ((not include ("1.8")) and (not include ("1.7")))
      }
      assert(caught5.getMessage === "\"1.7b\" did not include substring \"1.8\", but \"1.7b\" included substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "a1.7b" must (not include ("1.8") and not include ("1.7"))
      }
      assert(caught6.getMessage === "\"a1.7b\" did not include substring \"1.8\", but \"a1.7b\" included substring \"1.7\"")

      check(
        (s: String, t: String, u: String) =>
          (s + t + u).indexOf("hi") == -1 ==>
            throwsTestFailedException(s + t + u must (not include ("hi") and not include (t)))
      )
    }

    it("must throw TestFailedException if the string includes the specified substring when used in a logical-or expression used with not") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must (not include ("1.7") or (not include ("1.7")))
      }
      assert(caught1.getMessage === "\"1.7\" included substring \"1.7\", and \"1.7\" included substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must ((not include ("1.7")) or (not include ("1.7")))
      }
      assert(caught2.getMessage === "\"1.7\" included substring \"1.7\", and \"1.7\" included substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" must (not include ("1.7") or not include ("1.7"))
      }
      assert(caught3.getMessage === "\"1.7\" included substring \"1.7\", and \"1.7\" included substring \"1.7\"")

      val caught4 = intercept[TestFailedException] {
        "1.7" must (not (include ("1.7")) or not (include ("1.7")))
      }
      assert(caught4.getMessage === "\"1.7\" included substring \"1.7\", and \"1.7\" included substring \"1.7\"")

      val caught5 = intercept[TestFailedException] {
        "a1.7" must (not include ("1.7") or (not include ("1.7")))
      }
      assert(caught5.getMessage === "\"a1.7\" included substring \"1.7\", and \"a1.7\" included substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "1.7b" must ((not include ("1.7")) or (not include ("1.7")))
      }
      assert(caught6.getMessage === "\"1.7b\" included substring \"1.7\", and \"1.7b\" included substring \"1.7\"")

      val caught7 = intercept[TestFailedException] {
        "a1.7b" must (not include ("1.7") or not include ("1.7"))
      }
      assert(caught7.getMessage === "\"a1.7b\" included substring \"1.7\", and \"a1.7b\" included substring \"1.7\"")

      val caught8 = intercept[TestFailedException] {
        "a1.7b" must (not (include ("1.7")) or not (include ("1.7")))
      }
      assert(caught8.getMessage === "\"a1.7b\" included substring \"1.7\", and \"a1.7b\" included substring \"1.7\"")

      check(
        (s: String, t: String, u: String) =>
          throwsTestFailedException(s + t + u must (not include (s) or not include (t) or not include (u)))
      )
    }
  }
}

