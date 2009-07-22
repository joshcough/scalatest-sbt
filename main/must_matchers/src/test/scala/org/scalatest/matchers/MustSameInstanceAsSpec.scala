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

class MustSameInstanceAsSpec extends Spec with MustMatchers {

  describe("The be a ('symbol) syntax") {

    val string = "Hi"
    val obj: AnyRef = string
    val otherString = new String("Hi")

    it("must do nothing if the object is the same instance as another object") {
      string must be theSameInstanceAs (string)
      obj must be theSameInstanceAs (string)
      string must be theSameInstanceAs (obj)
    }

    it("must do nothing if the object is not the same instance as another object, when used with not") {
      otherString must not { be theSameInstanceAs (string) }
      otherString must not be theSameInstanceAs (string)
    }

    it("must do nothing if the object is the same instnace as another object, when used in a logical-and expression") {
      obj must ((be theSameInstanceAs (string)) and (be theSameInstanceAs (string)))
      obj must (be theSameInstanceAs (string) and (be theSameInstanceAs (string)))
      obj must (be theSameInstanceAs (string) and be theSameInstanceAs (string))
    }

    it("must do nothing if the object is the same instance as another object, when used in a logical-or expression") {

      obj must ((be theSameInstanceAs (otherString)) or (be theSameInstanceAs (string)))
      obj must (be theSameInstanceAs (otherString) or (be theSameInstanceAs (string)))
      obj must (be theSameInstanceAs (otherString) or be theSameInstanceAs (string))

      obj must ((be theSameInstanceAs (string)) or (be theSameInstanceAs (otherString)))
      obj must (be theSameInstanceAs (string) or (be theSameInstanceAs (otherString)))
      obj must (be theSameInstanceAs (string) or be theSameInstanceAs (otherString))
    }

    it("must do nothing if the object is not the same instance as another object, when used in a logical-and expression with not") {

      obj must (not (be theSameInstanceAs (otherString)) and not (be theSameInstanceAs (otherString)))
      obj must ((not be theSameInstanceAs (otherString)) and (not be theSameInstanceAs (otherString)))
      obj must (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (otherString))
    }

    it("must do nothing if the object is not the same instance as another object, when used in a logical-or expression with not") {

      obj must (not (be theSameInstanceAs (string)) or not (be theSameInstanceAs (otherString)))
      obj must ((not be theSameInstanceAs (string)) or (not be theSameInstanceAs (otherString)))
      obj must (not be theSameInstanceAs (string) or not be theSameInstanceAs (otherString))

      obj must (not (be theSameInstanceAs (otherString)) or not (be theSameInstanceAs (string)))
      obj must ((not be theSameInstanceAs (otherString)) or (not be theSameInstanceAs (string)))
      obj must (not be theSameInstanceAs (otherString) or not be theSameInstanceAs (string))
    }

    it("must throw TestFailedException if the object is not the same instance as another object") {
      val caught1 = intercept[TestFailedException] {
        otherString must be theSameInstanceAs (string)
      }
      assert(caught1.getMessage === "\"Hi\" was not the same instance as \"Hi\"")
    }

    it("must throw TestFailedException if the object is the same instance as another object, when used with not") {
      val caught1 = intercept[TestFailedException] {
        obj must not { be theSameInstanceAs (string) }
      }
      assert(caught1.getMessage === "\"Hi\" was the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj must not be theSameInstanceAs (string)
      }
      assert(caught2.getMessage === "\"Hi\" was the same instance as \"Hi\"")
    }

    it("must throw TestFailedException if the object is not the same instance as another object, when used in a logical-and expression") {
      val caught1 = intercept[TestFailedException] {
        obj must ((be theSameInstanceAs (string)) and (be theSameInstanceAs (otherString)))
      }
      assert(caught1.getMessage === "\"Hi\" was the same instance as \"Hi\", but \"Hi\" was not the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj must (be theSameInstanceAs (string) and (be theSameInstanceAs (otherString)))
      }
      assert(caught2.getMessage === "\"Hi\" was the same instance as \"Hi\", but \"Hi\" was not the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj must (be theSameInstanceAs (string) and be theSameInstanceAs (otherString))
      }
      assert(caught3.getMessage === "\"Hi\" was the same instance as \"Hi\", but \"Hi\" was not the same instance as \"Hi\"")
    }

    it("must throw TestFailedException if the object is not the same instance as another object, when used in a logical-or expression") {

      val caught1 = intercept[TestFailedException] {
        obj must ((be theSameInstanceAs (otherString)) or (be theSameInstanceAs (otherString)))
      }
      assert(caught1.getMessage === "\"Hi\" was not the same instance as \"Hi\", and \"Hi\" was not the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj must (be theSameInstanceAs (otherString) or (be theSameInstanceAs (otherString)))
      }
      assert(caught2.getMessage === "\"Hi\" was not the same instance as \"Hi\", and \"Hi\" was not the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj must (be theSameInstanceAs (otherString) or be theSameInstanceAs (otherString))
      }
      assert(caught3.getMessage === "\"Hi\" was not the same instance as \"Hi\", and \"Hi\" was not the same instance as \"Hi\"")
    }

    it("must throw TestFailedException if the object is the same instance as another object, when used in a logical-and expression with not") {

      val caught1 = intercept[TestFailedException] {
        obj must (not (be theSameInstanceAs (otherString)) and not (be theSameInstanceAs (string)))
      }
      assert(caught1.getMessage === "\"Hi\" was not the same instance as \"Hi\", but \"Hi\" was the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj must ((not be theSameInstanceAs (otherString)) and (not be theSameInstanceAs (string)))
      }
      assert(caught2.getMessage === "\"Hi\" was not the same instance as \"Hi\", but \"Hi\" was the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj must (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (string))
      }
      assert(caught3.getMessage === "\"Hi\" was not the same instance as \"Hi\", but \"Hi\" was the same instance as \"Hi\"")
      // Check that the error message "short circuits"
      val caught7 = intercept[TestFailedException] {
        obj must (not (be theSameInstanceAs (string)) and not (be theSameInstanceAs (otherString)))
      }
      assert(caught7.getMessage === "\"Hi\" was the same instance as \"Hi\"")
    }

    it("must throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-or expression with not") {

      val caught1 = intercept[TestFailedException] {
        obj must (not (be theSameInstanceAs (string)) or not (be theSameInstanceAs (string)))
      }
      assert(caught1.getMessage === "\"Hi\" was the same instance as \"Hi\", and \"Hi\" was the same instance as \"Hi\"")
      val caught2 = intercept[TestFailedException] {
        obj must ((not be theSameInstanceAs (string)) or (not be theSameInstanceAs (string)))
      }
      assert(caught2.getMessage === "\"Hi\" was the same instance as \"Hi\", and \"Hi\" was the same instance as \"Hi\"")
      val caught3 = intercept[TestFailedException] {
        obj must (not be theSameInstanceAs (string) or not be theSameInstanceAs (string))
      }
      assert(caught3.getMessage === "\"Hi\" was the same instance as \"Hi\", and \"Hi\" was the same instance as \"Hi\"")
    }
  }
}
