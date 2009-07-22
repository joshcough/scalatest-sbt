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

class MustEndWithSubstringSpec extends Spec with MustMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  describe("The endWith substring syntax") {

    it("must do nothing if the string ends with the specified substring") {

      "1.78" must endWith (".78")
      "21.7" must endWith ("7")
      "21.78" must endWith ("21.78")
      check((s: String, t: String) => returnsNormally(s + t must endWith (t)))
    }

    it("must do nothing if the string does not end with the specified substring when used with not") {

      "eight" must not { endWith ("1.7") }
      "eight" must not endWith ("1.7")
      check((s: String, t: String) => !(s + t).endsWith(s) ==> returnsNormally(s + t must not (endWith (s))))
      check((s: String, t: String) => !(s + t).endsWith(s) ==> returnsNormally(s + t must not endWith (s)))
    }

    it("must do nothing if the string does not end with the specified substring when used in a logical-and expression") {

      "1.7b" must ((endWith ("1.7b")) and (endWith ("7b")))
      "1.7b" must (endWith ("1.7b") and (endWith ("7b")))
      "1.7b" must (endWith ("1.7b") and endWith ("7b"))

      check((s: String, t: String) => returnsNormally(s + t must (endWith (t) and endWith (""))))
    }

    it("must do nothing if the string does not end with the specified substring when used in a logical-or expression") {

      "1.7b" must (endWith ("hello") or (endWith ("1.7b")))
      "1.7b" must ((endWith ("hello")) or (endWith ("1.7b")))
      "1.7b" must (endWith ("hello") or endWith ("1.7b"))

      "1.7b" must (endWith ("hello") or (endWith ("7b")))
      "1.7b" must ((endWith ("hello")) or (endWith ("7b")))
      "1.7b" must (endWith ("hello") or endWith ("7b"))

      check((s: String, t: String) => returnsNormally(s + t must (endWith ("hi") or endWith (t))))
    }

    it("must do nothing if the string does not end with the specified substring when used in a logical-and expression with not") {

      "fred" must (not (endWith ("fre")) and not (endWith ("1.7")))
      "fred" must ((not endWith ("fre")) and (not endWith ("1.7")))
      "fred" must (not endWith ("fre") and not endWith ("1.7"))
      check((s: String) => !(s endsWith "bob") && !(s endsWith "1.7") ==> returnsNormally(s must (not endWith ("bob") and not endWith ("1.7"))))
    }

    it("must do nothing if the string does not end with the specified substring when used in a logical-or expression with not") {
      "fred" must (not (endWith ("fred")) or not (endWith ("1.7")))
      "fred" must ((not endWith ("fred")) or (not endWith ("1.7")))
      "fred" must (not endWith ("fred") or not endWith ("1.7"))
      check((s: String) => s.indexOf("a") != 0 || s.indexOf("b") != 0 ==> returnsNormally(s must (not endWith ("a") or not endWith ("b"))))
    }

    it("must throw TestFailedException if the string does not match the specified substring") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must endWith ("1.78")
      }
      assert(caught1.getMessage === "\"1.7\" did not end with substring \"1.78\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must endWith ("21.7")
      }
      assert(caught2.getMessage === "\"1.7\" did not end with substring \"21.7\"")

      val caught3 = intercept[TestFailedException] {
        "1.78" must endWith ("1.7")
      }
      assert(caught3.getMessage === "\"1.78\" did not end with substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "eight" must endWith ("1.7")
      }
      assert(caught6.getMessage === "\"eight\" did not end with substring \"1.7\"")

      val caught7 = intercept[TestFailedException] {
        "one.eight" must endWith ("1.7")
      }
      assert(caught7.getMessage === "\"one.eight\" did not end with substring \"1.7\"")

      val caught8 = intercept[TestFailedException] {
        "onedoteight" must endWith ("1.7")
      }
      assert(caught8.getMessage === "\"onedoteight\" did not end with substring \"1.7\"")

      val caught9 = intercept[TestFailedException] {
        "***" must endWith ("1.7")
      }
      assert(caught9.getMessage === "\"***\" did not end with substring \"1.7\"")

      check((s: String) => !(s endsWith "1.7") ==> throwsTestFailedException(s must endWith ("1.7")))
    }

    it("must throw TestFailedException if the string does matches the specified substring when used with not") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must not { endWith ("1.7") }
      }
      assert(caught1.getMessage === "\"1.7\" ended with substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must not { endWith ("7") }
      }
      assert(caught2.getMessage === "\"1.7\" ended with substring \"7\"")

      val caught3 = intercept[TestFailedException] {
        "-1.8" must not { endWith (".8") }
      }
      assert(caught3.getMessage === "\"-1.8\" ended with substring \".8\"")

      val caught4 = intercept[TestFailedException] {
        "8b" must not { endWith ("b") }
      }
      assert(caught4.getMessage === "\"8b\" ended with substring \"b\"")

      val caught5 = intercept[TestFailedException] {
        "1." must not { endWith ("1.") }
      }
      assert(caught5.getMessage === "\"1.\" ended with substring \"1.\"")

      val caught11 = intercept[TestFailedException] {
        "1.7" must not endWith (".7")
      }
      assert(caught11.getMessage === "\"1.7\" ended with substring \".7\"")

      val caught13 = intercept[TestFailedException] {
        "-1.8" must not endWith ("8")
      }
      assert(caught13.getMessage === "\"-1.8\" ended with substring \"8\"")

      val caught14 = intercept[TestFailedException] {
        "8" must not endWith ("")
      }
      assert(caught14.getMessage === "\"8\" ended with substring \"\"")

      val caught15 = intercept[TestFailedException] {
        "1." must not endWith ("1.")
      }
      assert(caught15.getMessage === "\"1.\" ended with substring \"1.\"")

      val caught21 = intercept[TestFailedException] {
        "1.7a" must not { endWith ("7a") }
      }
      assert(caught21.getMessage === "\"1.7a\" ended with substring \"7a\"")

      val caught22 = intercept[TestFailedException] {
        "b1.7" must not { endWith ("1.7") }
      }
      assert(caught22.getMessage === "\"b1.7\" ended with substring \"1.7\"")

      val caught23 = intercept[TestFailedException] {
        "ba-1.8" must not { endWith ("a-1.8") }
      }
      assert(caught23.getMessage === "\"ba-1.8\" ended with substring \"a-1.8\"")

      check((s: String) => s.length != 0 ==> throwsTestFailedException(s must not endWith (s.substring(s.length - 1, s.length))))
    }

    it("must throw TestFailedException if the string ends with the specified substring when used in a logical-and expression") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must (endWith ("1.7") and (endWith ("1.8")))
      }
      assert(caught1.getMessage === "\"1.7\" ended with substring \"1.7\", but \"1.7\" did not end with substring \"1.8\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must ((endWith ("7")) and (endWith ("1.8")))
      }
      assert(caught2.getMessage === "\"1.7\" ended with substring \"7\", but \"1.7\" did not end with substring \"1.8\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" must (endWith (".7") and endWith ("1.8"))
      }
      assert(caught3.getMessage === "\"1.7\" ended with substring \".7\", but \"1.7\" did not end with substring \"1.8\"")

      // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
      val caught4 = intercept[TestFailedException] {
        "one.eight" must (endWith ("1.7") and (endWith ("1.8")))
      }
      assert(caught4.getMessage === "\"one.eight\" did not end with substring \"1.7\"")

      val caught5 = intercept[TestFailedException] {
        "one.eight" must ((endWith ("1.7")) and (endWith ("1.8")))
      }
      assert(caught5.getMessage === "\"one.eight\" did not end with substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "one.eight" must (endWith ("1.7") and endWith ("1.8"))
      }
      assert(caught6.getMessage === "\"one.eight\" did not end with substring \"1.7\"")

      check((s: String, t: String, u: String) => !((s + u) endsWith t) ==> throwsTestFailedException(s + u must (endWith (u) and endWith (t))))
    }

    it("must throw TestFailedException if the string ends with the specified substring when used in a logical-or expression") {

      val caught1 = intercept[TestFailedException] {
        "one.seven" must (endWith ("1.7") or (endWith ("1.8")))
      }
      assert(caught1.getMessage === "\"one.seven\" did not end with substring \"1.7\", and \"one.seven\" did not end with substring \"1.8\"")

      val caught2 = intercept[TestFailedException] {
        "one.seven" must ((endWith ("1.7")) or (endWith ("1.8")))
      }
      assert(caught2.getMessage === "\"one.seven\" did not end with substring \"1.7\", and \"one.seven\" did not end with substring \"1.8\"")

      val caught3 = intercept[TestFailedException] {
        "one.seven" must (endWith ("1.7") or endWith ("1.8"))
      }
      assert(caught3.getMessage === "\"one.seven\" did not end with substring \"1.7\", and \"one.seven\" did not end with substring \"1.8\"")

      check(
        (s: String, t: String, u: String, v: String) => {
          (t.length != 0 && v.length != 0 && !(s + u).endsWith(t) && !(s + u).endsWith(v)) ==>
            throwsTestFailedException(s + u must (endWith (t) or endWith (v)))
        }
      )
    }

    it("must throw TestFailedException if the string ends with the specified substring when used in a logical-and expression used with not") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must (not endWith ("1.8") and (not endWith ("1.7")))
      }
      assert(caught1.getMessage === "\"1.7\" did not end with substring \"1.8\", but \"1.7\" ended with substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must ((not endWith ("1.8")) and (not endWith ("1.7")))
      }
      assert(caught2.getMessage === "\"1.7\" did not end with substring \"1.8\", but \"1.7\" ended with substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" must (not endWith ("1.8") and not endWith ("1.7"))
      }
      assert(caught3.getMessage === "\"1.7\" did not end with substring \"1.8\", but \"1.7\" ended with substring \"1.7\"")

      val caught4 = intercept[TestFailedException] {
        "a1.7" must (not endWith ("1.8") and (not endWith ("a1.7")))
      }
      assert(caught4.getMessage === "\"a1.7\" did not end with substring \"1.8\", but \"a1.7\" ended with substring \"a1.7\"")

      val caught5 = intercept[TestFailedException] {
        "b1.7" must ((not endWith ("1.8")) and (not endWith ("1.7")))
      }
      assert(caught5.getMessage === "\"b1.7\" did not end with substring \"1.8\", but \"b1.7\" ended with substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "a1.7b" must (not endWith ("1.8") and not endWith ("1.7b"))
      }
      assert(caught6.getMessage === "\"a1.7b\" did not end with substring \"1.8\", but \"a1.7b\" ended with substring \"1.7b\"")

      check(
        (s: String, t: String, u: String) =>
          (s + t + u).indexOf("hi") != 0 ==>
            throwsTestFailedException(s + t + u must (not endWith ("hi") and not endWith (u)))
      )
    }

    it("must throw TestFailedException if the string ends with the specified substring when used in a logical-or expression used with not") {

      val caught1 = intercept[TestFailedException] {
        "1.7" must (not endWith ("1.7") or (not endWith ("1.7")))
      }
      assert(caught1.getMessage === "\"1.7\" ended with substring \"1.7\", and \"1.7\" ended with substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" must ((not endWith ("1.7")) or (not endWith ("1.7")))
      }
      assert(caught2.getMessage === "\"1.7\" ended with substring \"1.7\", and \"1.7\" ended with substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" must (not endWith ("1.7") or not endWith ("1.7"))
      }
      assert(caught3.getMessage === "\"1.7\" ended with substring \"1.7\", and \"1.7\" ended with substring \"1.7\"")

      val caught4 = intercept[TestFailedException] {
        "1.7" must (not (endWith ("1.7")) or not (endWith ("1.7")))
      }
      assert(caught4.getMessage === "\"1.7\" ended with substring \"1.7\", and \"1.7\" ended with substring \"1.7\"")

      val caught5 = intercept[TestFailedException] {
        "a1.7" must (not endWith (".7") or (not endWith ("a1.7")))
      }
      assert(caught5.getMessage === "\"a1.7\" ended with substring \".7\", and \"a1.7\" ended with substring \"a1.7\"")

      val caught6 = intercept[TestFailedException] {
        "b1.7" must ((not endWith ("1.7")) or (not endWith ("1.7")))
      }
      assert(caught6.getMessage === "\"b1.7\" ended with substring \"1.7\", and \"b1.7\" ended with substring \"1.7\"")

      val caught7 = intercept[TestFailedException] {
        "a1.7b" must (not endWith ("1.7b") or not endWith ("7b"))
      }
      assert(caught7.getMessage === "\"a1.7b\" ended with substring \"1.7b\", and \"a1.7b\" ended with substring \"7b\"")

      val caught8 = intercept[TestFailedException] {
        "a1.7b" must (not (endWith ("1.7b")) or not (endWith ("7b")))
      }
      assert(caught8.getMessage === "\"a1.7b\" ended with substring \"1.7b\", and \"a1.7b\" ended with substring \"7b\"")

      check(
        (s: String, t: String) =>
          throwsTestFailedException(s + t must (not endWith (t) or not endWith ("")))
      )
    }
  }
}

