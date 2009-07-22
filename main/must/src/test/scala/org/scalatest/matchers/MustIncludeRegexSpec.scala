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

class MustIncludeRegexSpec extends Spec with MustMatchers with Checkers with ReturnsNormallyThrowsAssertion {

/*
s must include substring t
s must include regex t
s must startWith substring t
s must startWith regex t
s must endWith substring t
s must endWith regex t
s must fullyMatch regex t
*/

  describe("The include regex syntax") {

    val decimal = """(-)?(\d+)(\.\d*)?"""
    val decimalRegex = """(-)?(\d+)(\.\d*)?""".r

    describe("(when the regex is specified by a string)") {

      it("must do nothing if the string includes substring that matched regex specified as a string") {

        "1.78" must include regex ("1.7")
        "21.7" must include regex ("1.7")
        "21.78" must include regex ("1.7")
        "1.7" must include regex (decimal)
        "21.7" must include regex (decimal)
        "1.78" must include regex (decimal)
        "a -1.8 difference" must include regex (decimal)
        "b8" must include regex (decimal)
        "8x" must include regex (decimal)
        "1.x" must include regex (decimal)

        // The remaining are full matches, which must also work with "include"
        "1.7" must include regex ("1.7")
        "1.7" must include regex (decimal)
        "-1.8" must include regex (decimal)
        "8" must include regex (decimal)
        "1." must include regex (decimal)
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used with not") {

        "eight" must not { include regex (decimal) }
        "one.eight" must not { include regex (decimal) }

        "eight" must not include regex (decimal)
        "one.eight" must not include regex (decimal)
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression") {

        "a1.7" must (include regex (decimal) and (include regex (decimal)))
        "a1.7" must (include regex (decimal) and (include regex (decimal)))
        "a1.7" must (include regex (decimal) and (include regex (decimal)))

        "1.7b" must ((include regex (decimal)) and (include regex (decimal)))
        "1.7b" must ((include regex (decimal)) and (include regex (decimal)))
        "1.7b" must ((include regex (decimal)) and (include regex (decimal)))

        "a1.7b" must (include regex (decimal) and include regex (decimal))
        "a1.7b" must (include regex (decimal) and include regex (decimal))
        "a1.7b" must (include regex (decimal) and include regex (decimal))

        "1.7" must (include regex (decimal) and (include regex (decimal)))
        "1.7" must ((include regex (decimal)) and (include regex (decimal)))
        "1.7" must (include regex (decimal) and include regex (decimal))
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression") {

        "a1.7" must (include regex ("hello") or (include regex (decimal)))
        "a1.7" must (include regex ("hello") or (include regex (decimal)))
        "a1.7" must (include regex ("hello") or (include regex (decimal)))

        "1.7b" must ((include regex ("hello")) or (include regex (decimal)))
        "1.7b" must ((include regex ("hello")) or (include regex (decimal)))
        "a1.7b" must ((include regex ("hello")) or (include regex (decimal)))

        "a1.7b" must (include regex ("hello") or include regex (decimal))
        "a1.7b" must (include regex ("hello") or include regex (decimal))
        "a1.7b" must (include regex ("hello") or include regex (decimal))
  
        "1.7" must (include regex ("hello") or (include regex (decimal)))
        "1.7" must ((include regex ("hello")) or (include regex (decimal)))
        "1.7" must (include regex ("hello") or include regex (decimal))
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression with not") {
        "fred" must (not (include regex ("bob")) and not (include regex (decimal)))
        "fred" must ((not include regex ("bob")) and (not include regex (decimal)))
        "fred" must (not include regex ("bob") and not include regex (decimal))
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression with not") {
        "fred" must (not (include regex ("fred")) or not (include regex (decimal)))
        "fred" must ((not include regex ("fred")) or (not include regex (decimal)))
        "fred" must (not include regex ("fred") or not include regex (decimal))
      }
  
      it("must throw TestFailedException if the string does not match substring that matched regex specified as a string") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must include regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must include regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" must include regex (decimal)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" must include regex (decimal)
        }
        assert(caught6.getMessage === "\"eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "one.eight" must include regex (decimal)
        }
        assert(caught7.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" must include regex (decimal)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" must include regex (decimal)
        }
        assert(caught9.getMessage === "\"***\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      it("must throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must not { include regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must not { include regex (decimal) }
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" must not { include regex (decimal) }
        }
        assert(caught3.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" must not { include regex (decimal) }
        }
        assert(caught4.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." must not { include regex (decimal) }
        }
        assert(caught5.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" must not include regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" must not include regex (decimal)
        }
        assert(caught12.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" must not include regex (decimal)
        }
        assert(caught13.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" must not include regex (decimal)
        }
        assert(caught14.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." must not include regex (decimal)
        }
        assert(caught15.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "a1.7" must not { include regex ("1.7") }
        }
        assert(caught21.getMessage === "\"a1.7\" included substring that matched regex 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "1.7b" must not { include regex (decimal) }
        }
        assert(caught22.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "a-1.8b" must not { include regex (decimal) }
        }
        assert(caught23.getMessage === "\"a-1.8b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }

      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must (include regex (decimal) and (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must ((include regex (decimal)) and (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" must (include regex (decimal) and include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "one.eight" must (include regex (decimal) and (include regex ("1.8")))
        }
        assert(caught4.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "one.eight" must ((include regex (decimal)) and (include regex ("1.8")))
        }
        assert(caught5.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "one.eight" must (include regex (decimal) and include regex ("1.8"))
        }
        assert(caught6.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression") {
  
        val caught1 = intercept[TestFailedException] {
          "one.seven" must (include regex (decimal) or (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "one.seven" must ((include regex (decimal)) or (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "one.seven" must (include regex (decimal) or include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
      }
  
      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "1.7" must (not include regex ("1.8") and (not include regex (decimal)))
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" must ((not include regex ("1.8")) and (not include regex (decimal)))
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" must (not include regex ("1.8") and not include regex (decimal))
        }
        assert(caught3.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7" must (not include regex ("1.8") and (not include regex (decimal)))
        }
        assert(caught4.getMessage === "\"a1.7\" did not include substring that matched regex 1.8, but \"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7b" must ((not include regex ("1.8")) and (not include regex (decimal)))
        }
        assert(caught5.getMessage === "\"1.7b\" did not include substring that matched regex 1.8, but \"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught6 = intercept[TestFailedException] {
          "a1.7b" must (not include regex ("1.8") and not include regex (decimal))
        }
        assert(caught6.getMessage === "\"a1.7b\" did not include substring that matched regex 1.8, but \"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }

      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must (not include regex (decimal) or (not include regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must ((not include regex (decimal)) or (not include regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" must (not include regex (decimal) or not include regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" must (not (include regex (decimal)) or not (include regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught5 = intercept[TestFailedException] {
          "a1.7" must (not include regex (decimal) or (not include regex ("1.7")))
        }
        assert(caught5.getMessage === "\"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" included substring that matched regex 1.7")
  
        val caught6 = intercept[TestFailedException] {
          "1.7b" must ((not include regex (decimal)) or (not include regex ("1.7")))
        }
        assert(caught6.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7b\" included substring that matched regex 1.7")
  
        val caught7 = intercept[TestFailedException] {
          "a1.7b" must (not include regex (decimal) or not include regex ("1.7"))
        }
        assert(caught7.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
  
        val caught8 = intercept[TestFailedException] {
          "a1.7b" must (not (include regex (decimal)) or not (include regex ("1.7")))
        }
        assert(caught8.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
      }
    }

    describe("(when the regex is specified by an actual Regex)") {

      it("must do nothing if the string includes substring that matched regex specified as a string") {

        "1.78" must include regex ("1.7")
        "21.7" must include regex ("1.7")
        "21.78" must include regex ("1.7")
        "1.7" must include regex (decimalRegex)
        "21.7" must include regex (decimalRegex)
        "1.78" must include regex (decimalRegex)
        "a -1.8 difference" must include regex (decimalRegex)
        "b8" must include regex (decimalRegex)
        "8x" must include regex (decimalRegex)
        "1.x" must include regex (decimalRegex)

        // The remaining are full matches, which must also work with "include"
        "1.7" must include regex ("1.7")
        "1.7" must include regex (decimalRegex)
        "-1.8" must include regex (decimalRegex)
        "8" must include regex (decimalRegex)
        "1." must include regex (decimalRegex)
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used with not") {

        "eight" must not { include regex (decimalRegex) }
        "one.eight" must not { include regex (decimalRegex) }

        "eight" must not include regex (decimalRegex)
        "one.eight" must not include regex (decimalRegex)
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression") {

        "a1.7" must (include regex (decimalRegex) and (include regex (decimalRegex)))
        "1.7b" must (include regex (decimalRegex) and (include regex (decimalRegex)))
        "a1.7b" must (include regex (decimalRegex) and (include regex (decimalRegex)))

        "a1.7" must ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "1.7b" must ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "a1.7b" must ((include regex (decimalRegex)) and (include regex (decimalRegex)))

        "a1.7" must (include regex (decimalRegex) and include regex (decimalRegex))
        "1.7b" must (include regex (decimalRegex) and include regex (decimalRegex))
        "a1.7b" must (include regex (decimalRegex) and include regex (decimalRegex))

        "1.7" must (include regex (decimalRegex) and (include regex (decimalRegex)))
        "1.7" must ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "1.7" must (include regex (decimalRegex) and include regex (decimalRegex))
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression") {

        "a1.7" must (include regex ("hello") or (include regex (decimalRegex)))
        "1.7b" must (include regex ("hello") or (include regex (decimalRegex)))
        "a1.7b" must (include regex ("hello") or (include regex (decimalRegex)))

        "a1.7" must ((include regex ("hello")) or (include regex (decimalRegex)))
        "1.7b" must ((include regex ("hello")) or (include regex (decimalRegex)))
        "a1.7b" must ((include regex ("hello")) or (include regex (decimalRegex)))

        "a1.7" must (include regex ("hello") or include regex (decimalRegex))
        "1.7b" must (include regex ("hello") or include regex (decimalRegex))
        "a1.7b" must (include regex ("hello") or include regex (decimalRegex))
  
        "1.7" must (include regex ("hello") or (include regex (decimalRegex)))
        "1.7" must ((include regex ("hello")) or (include regex (decimalRegex)))
        "1.7" must (include regex ("hello") or include regex (decimalRegex))
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression with not") {
        "fred" must (not (include regex ("bob")) and not (include regex (decimalRegex)))
        "fred" must ((not include regex ("bob")) and (not include regex (decimalRegex)))
        "fred" must (not include regex ("bob") and not include regex (decimalRegex))
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression with not") {
        "fred" must (not (include regex ("fred")) or not (include regex (decimalRegex)))
        "fred" must ((not include regex ("fred")) or (not include regex (decimalRegex)))
        "fred" must (not include regex ("fred") or not include regex (decimalRegex))
      }
  
      it("must throw TestFailedException if the string does not match substring that matched regex specified as a string") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must include regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must include regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" must include regex (decimalRegex)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" must include regex (decimalRegex)
        }
        assert(caught6.getMessage === "\"eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "one.eight" must include regex (decimalRegex)
        }
        assert(caught7.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" must include regex (decimalRegex)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" must include regex (decimalRegex)
        }
        assert(caught9.getMessage === "\"***\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      it("must throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must not { include regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must not { include regex (decimalRegex) }
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" must not { include regex (decimalRegex) }
        }
        assert(caught3.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" must not { include regex (decimalRegex) }
        }
        assert(caught4.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." must not { include regex (decimalRegex) }
        }
        assert(caught5.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" must not include regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" must not include regex (decimalRegex)
        }
        assert(caught12.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" must not include regex (decimalRegex)
        }
        assert(caught13.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" must not include regex (decimalRegex)
        }
        assert(caught14.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." must not include regex (decimalRegex)
        }
        assert(caught15.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "a1.7" must not { include regex ("1.7") }
        }
        assert(caught21.getMessage === "\"a1.7\" included substring that matched regex 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "1.7b" must not { include regex (decimalRegex) }
        }
        assert(caught22.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "a-1.8b" must not { include regex (decimalRegex) }
        }
        assert(caught23.getMessage === "\"a-1.8b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }

      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must (include regex (decimalRegex) and (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must ((include regex (decimalRegex)) and (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" must (include regex (decimalRegex) and include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "one.eight" must (include regex (decimalRegex) and (include regex ("1.8")))
        }
        assert(caught4.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "one.eight" must ((include regex (decimalRegex)) and (include regex ("1.8")))
        }
        assert(caught5.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "one.eight" must (include regex (decimalRegex) and include regex ("1.8"))
        }
        assert(caught6.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression") {
  
        val caught1 = intercept[TestFailedException] {
          "one.seven" must (include regex (decimalRegex) or (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "one.seven" must ((include regex (decimalRegex)) or (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "one.seven" must (include regex (decimalRegex) or include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
      }
  
      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "1.7" must (not include regex ("1.8") and (not include regex (decimalRegex)))
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" must ((not include regex ("1.8")) and (not include regex (decimalRegex)))
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" must (not include regex ("1.8") and not include regex (decimalRegex))
        }
        assert(caught3.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7" must (not include regex ("1.8") and (not include regex (decimalRegex)))
        }
        assert(caught4.getMessage === "\"a1.7\" did not include substring that matched regex 1.8, but \"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7b" must ((not include regex ("1.8")) and (not include regex (decimalRegex)))
        }
        assert(caught5.getMessage === "\"1.7b\" did not include substring that matched regex 1.8, but \"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught6 = intercept[TestFailedException] {
          "a1.7b" must (not include regex ("1.8") and not include regex (decimalRegex))
        }
        assert(caught6.getMessage === "\"a1.7b\" did not include substring that matched regex 1.8, but \"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }

      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" must (not include regex (decimalRegex) or (not include regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" must ((not include regex (decimalRegex)) or (not include regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" must (not include regex (decimalRegex) or not include regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" must (not (include regex (decimalRegex)) or not (include regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught5 = intercept[TestFailedException] {
          "a1.7" must (not include regex (decimalRegex) or (not include regex ("1.7")))
        }
        assert(caught5.getMessage === "\"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" included substring that matched regex 1.7")
  
        val caught6 = intercept[TestFailedException] {
          "1.7b" must ((not include regex (decimalRegex)) or (not include regex ("1.7")))
        }
        assert(caught6.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7b\" included substring that matched regex 1.7")
  
        val caught7 = intercept[TestFailedException] {
          "a1.7b" must (not include regex (decimalRegex) or not include regex ("1.7"))
        }
        assert(caught7.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
  
        val caught8 = intercept[TestFailedException] {
          "a1.7b" must (not (include regex (decimalRegex)) or not (include regex ("1.7")))
        }
        assert(caught8.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
      }
    }
  }
}



/*
      it("must do nothing if the string includes substring that matched regex specified as a string") {
        "1.78" must include regex ("1.7")
        "21.7" must include regex ("1.7")
        "21.78" must include regex ("1.7")
        "1.7" must include regex (decimalRegex)
        "21.7" must include regex (decimalRegex)
        "1.78" must include regex (decimalRegex)
        "a -1.8 difference" must include regex (decimalRegex)
        "b8" must include regex (decimalRegex)
        "8x" must include regex (decimalRegex)
        "1.x" must include regex (decimalRegex)
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression") {

        "a1.7" must (include regex (decimalRegex) and (include regex (decimalRegex)))
        "1.7b" must (include regex (decimalRegex) and (include regex (decimalRegex)))
        "a1.7b" must (include regex (decimalRegex) and (include regex (decimalRegex)))

        "a1.7" must ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "1.7b" must ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "a1.7b" must ((include regex (decimalRegex)) and (include regex (decimalRegex)))

        "a1.7" must (include regex (decimalRegex) and include regex (decimalRegex))
        "1.7b" must (include regex (decimalRegex) and include regex (decimalRegex))
        "a1.7b" must (include regex (decimalRegex) and include regex (decimalRegex))
      }
  
      it("must do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression") {

        "a1.7" must (include regex ("hello") or (include regex (decimalRegex)))
        "1.7b" must (include regex ("hello") or (include regex (decimalRegex)))
        "a1.7b" must (include regex ("hello") or (include regex (decimalRegex)))

        "a1.7" must ((include regex ("hello")) or (include regex (decimalRegex)))
        "1.7b" must ((include regex ("hello")) or (include regex (decimalRegex)))
        "a1.7b" must ((include regex ("hello")) or (include regex (decimalRegex)))

        "a1.7" must (include regex ("hello") or include regex (decimalRegex))
        "1.7b" must (include regex ("hello") or include regex (decimalRegex))
        "a1.7b" must (include regex ("hello") or include regex (decimalRegex))
      }
  
      it("must throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "a1.7" must not { include regex ("1.7") }
        }
        assert(caught1.getMessage === "\"a1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7b" must not { include regex (decimalRegex) }
        }
        assert(caught2.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "a-1.8b" must not { include regex (decimalRegex) }
        }
        assert(caught3.getMessage === "\"a-1.8b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "a1.7" must (not include regex ("1.8") and (not include regex (decimalRegex)))
        }
        assert(caught1.getMessage === "\"a1.7\" did not include substring that matched regex 1.8, but \"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7b" must ((not include regex ("1.8")) and (not include regex (decimalRegex)))
        }
        assert(caught2.getMessage === "\"1.7b\" did not include substring that matched regex 1.8, but \"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "a1.7b" must (not include regex ("1.8") and not include regex (decimalRegex))
        }
        assert(caught3.getMessage === "\"a1.7b\" did not include substring that matched regex 1.8, but \"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      it("must throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "a1.7" must (not include regex (decimalRegex) or (not include regex ("1.7")))
        }
        assert(caught1.getMessage === "\"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7b" must ((not include regex (decimalRegex)) or (not include regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7b\" included substring that matched regex 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "a1.7b" must (not include regex (decimalRegex) or not include regex ("1.7"))
        }
        assert(caught3.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7b" must (not (include regex (decimalRegex)) or not (include regex ("1.7")))
        }
        assert(caught4.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
      }
*/
