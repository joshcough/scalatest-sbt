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

class MustContainElementSpec extends Spec with MustMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  // Checking for a specific size
  describe("The 'contain (Int)' syntax") {

    describe("on Array") {

      it("must do nothing if array contains the specified element") {
        Array(1, 2) must contain (2)
        Array(1, 2) must (contain (2))
        check((arr: Array[Int]) => arr.size != 0 ==> returnsNormally(arr must contain (arr(arr.length - 1))))
      }

      it("must do nothing if array does not contain the element and used with must not") {
        Array(1, 2) must not { contain (3) }
        Array(1, 2) must not contain (3)
        check((arr: Array[Int], i: Int) => !arr.exists(_ == i) ==> returnsNormally(arr must not { contain (i) }))
        check((arr: Array[Int], i: Int) => !arr.exists(_ == i) ==> returnsNormally(arr must not contain (i)))
      }

      it("must do nothing when array contains the specified element and used in a logical-and expression") {
        Array(1, 2) must { contain (2) and (contain (1)) }
        Array(1, 2) must ((contain (2)) and (contain (1)))
        Array(1, 2) must (contain (2) and contain (1))
       }

      it("must do nothing when array contains the specified element and used in a logical-or expression") {
        Array(1, 2) must { contain (77) or (contain (2)) }
        Array(1, 2) must ((contain (77)) or (contain (2)))
        Array(1, 2) must (contain (77) or contain (2))
      }

      it("must do nothing when array doesn't contain the specified element and used in a logical-and expression with not") {
        Array(1, 2) must { not { contain (5) } and not { contain (3) }}
        Array(1, 2) must ((not contain (5)) and (not contain (3)))
        Array(1, 2) must (not contain (5) and not contain (3))
      }

      it("must do nothing when array doesn't contain the specified element and used in a logical-or expression with not") {
        Array(1, 2) must { not { contain (1) } or not { contain (3) }}
        Array(1, 2) must ((not contain (1)) or (not contain (3)))
        Array(1, 2) must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if array does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          Array(1, 2) must contain (3)
        }
        assert(caught.getMessage === "Array(1, 2) did not contain element 3")
        check((arr: Array[String], s: String) => !arr.exists(_ == s) ==> throwsTestFailedException(arr must contain (s)))
      }

      it("must throw TestFailedException if array contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) must not contain (2)
        }
        assert(caught1.getMessage === "Array(1, 2) contained element 2")
        check((arr: Array[String]) => arr.length > 0 ==> throwsTestFailedException(arr must not contain (arr(0))))

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) must not (contain (2))
        }
        assert(caught2.getMessage === "Array(1, 2) contained element 2")
        check((arr: Array[String]) => arr.length > 0 ==> throwsTestFailedException(arr must not (contain (arr(0)))))

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) must (not contain (2))
        }
        assert(caught3.getMessage === "Array(1, 2) contained element 2")
        check((arr: Array[String]) => arr.length > 0 ==> throwsTestFailedException(arr must not (contain (arr(0)))))
      }

      it("must throw a TestFailedException when array doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Array(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Array(1, 2) did not contain element 5")
      }

      it("must throw a TestFailedException when array doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Array(1, 2) did not contain element 55, and Array(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Array(1, 2) did not contain element 55, and Array(1, 2) did not contain element 22")
      }

      it("must throw a TestFailedException when array contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Array(1, 2) did not contain element 3, but Array(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Array(1, 2) did not contain element 3, but Array(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Array(1, 2) did not contain element 3, but Array(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when array contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Array(1, 2) contained element 2, and Array(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Array(1, 2) contained element 2, and Array(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Array(1, 2) contained element 2, and Array(1, 2) contained element 2")
      }
    }

    describe("on scala.collection.immutable.Set") {

      it("must do nothing if set contains the specified element") {
        Set(1, 2) must contain (2)
        Set(1, 2) must (contain (2))
      }

      it("must do nothing if set does not contain the element and used with must not") {
        Set(1, 2) must not { contain (3) }
        Set(1, 2) must not contain (3)
      }

      it("must do nothing when set contains the specified element and used in a logical-and expression") {
        Set(1, 2) must { contain (2) and (contain (1)) }
        Set(1, 2) must ((contain (2)) and (contain (1)))
        Set(1, 2) must (contain (2) and contain (1))
       }

      it("must do nothing when set contains the specified element and used in a logical-or expression") {
        Set(1, 2) must { contain (77) or (contain (2)) }
        Set(1, 2) must ((contain (77)) or (contain (2)))
        Set(1, 2) must (contain (77) or contain (2))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        Set(1, 2) must { not { contain (5) } and not { contain (3) }}
        Set(1, 2) must ((not contain (5)) and (not contain (3)))
        Set(1, 2) must (not contain (5) and not contain (3))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        Set(1, 2) must { not { contain (1) } or not { contain (3) }}
        Set(1, 2) must ((not contain (1)) or (not contain (3)))
        Set(1, 2) must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          Set(1, 2) must contain (3)
        }
        assert(caught.getMessage === "Set(1, 2) did not contain element 3")
      }

      it("must throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) must not contain (2)
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) must not (contain (2))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Set(1, 2) must (not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 5")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Set(1, 2) must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Set(1, 2) must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")
      }
    }

    describe("on scala.collection.mutable.Set") {

      import scala.collection.mutable

      it("must do nothing if set contains the specified element") {
        mutable.Set(1, 2) must contain (2)
        mutable.Set(1, 2) must (contain (2))
      }

      it("must do nothing if set does not contain the element and used with must not") {
        mutable.Set(1, 2) must not { contain (3) }
        mutable.Set(1, 2) must not contain (3)
      }

      it("must do nothing when set contains the specified element and used in a logical-and expression") {
        mutable.Set(1, 2) must { contain (2) and (contain (1)) }
        mutable.Set(1, 2) must ((contain (2)) and (contain (1)))
        mutable.Set(1, 2) must (contain (2) and contain (1))
       }

      it("must do nothing when set contains the specified element and used in a logical-or expression") {
        mutable.Set(1, 2) must { contain (77) or (contain (2)) }
        mutable.Set(1, 2) must ((contain (77)) or (contain (2)))
        mutable.Set(1, 2) must (contain (77) or contain (2))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        mutable.Set(1, 2) must { not { contain (5) } and not { contain (3) }}
        mutable.Set(1, 2) must ((not contain (5)) and (not contain (3)))
        mutable.Set(1, 2) must (not contain (5) and not contain (3))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        mutable.Set(1, 2) must { not { contain (1) } or not { contain (3) }}
        mutable.Set(1, 2) must ((not contain (1)) or (not contain (3)))
        mutable.Set(1, 2) must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          mutable.Set(1, 2) must contain (3)
        }
        assert(caught.getMessage === "Set(1, 2) did not contain element 3")
      }

      it("must throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Set(1, 2) must not contain (2)
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          mutable.Set(1, 2) must not (contain (2))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          mutable.Set(1, 2) must (not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.Set(1, 2) must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          mutable.Set(1, 2) must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 5")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.Set(1, 2) must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          mutable.Set(1, 2) must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Set(1, 2) must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          mutable.Set(1, 2) must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          mutable.Set(1, 2) must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Set(1, 2) must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          mutable.Set(1, 2) must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          mutable.Set(1, 2) must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")
      }
    }

    describe("on scala.collection.Set") {

      val set: scala.collection.Set[Int] = Set(1, 2)

      it("must do nothing if set contains the specified element") {
        set must contain (2)
        set must (contain (2))
      }

      it("must do nothing if set does not contain the element and used with must not") {
        set must not { contain (3) }
        set must not contain (3)
      }

      it("must do nothing when set contains the specified element and used in a logical-and expression") {
        set must { contain (2) and (contain (1)) }
        set must ((contain (2)) and (contain (1)))
        set must (contain (2) and contain (1))
       }

      it("must do nothing when set contains the specified element and used in a logical-or expression") {
        set must { contain (77) or (contain (2)) }
        set must ((contain (77)) or (contain (2)))
        set must (contain (77) or contain (2))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        set must { not { contain (5) } and not { contain (3) }}
        set must ((not contain (5)) and (not contain (3)))
        set must (not contain (5) and not contain (3))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        set must { not { contain (1) } or not { contain (3) }}
        set must ((not contain (1)) or (not contain (3)))
        set must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          set must contain (3)
        }
        assert(caught.getMessage === "Set(1, 2) did not contain element 3")
      }

      it("must throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          set must not contain (2)
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          set must not (contain (2))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          set must (not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          set must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          set must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 5")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          set must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          set must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          set must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          set must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          set must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          set must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          set must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          set must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")
      }
    }

    describe("on scala.collection.immutable.HashSet") {

      import scala.collection.immutable.HashSet
        
      it("must do nothing if set contains the specified element") {
        HashSet(1, 2) must contain (2)
        HashSet(1, 2) must (contain (2))
      }

      it("must do nothing if set does not contain the element and used with must not") {
        HashSet(1, 2) must not { contain (3) }
        HashSet(1, 2) must not contain (3)
      }

      it("must do nothing when set contains the specified element and used in a logical-and expression") {
        HashSet(1, 2) must { contain (2) and (contain (1)) }
        HashSet(1, 2) must ((contain (2)) and (contain (1)))
        HashSet(1, 2) must (contain (2) and contain (1))
       }

      it("must do nothing when set contains the specified element and used in a logical-or expression") {
        HashSet(1, 2) must { contain (77) or (contain (2)) }
        HashSet(1, 2) must ((contain (77)) or (contain (2)))
        HashSet(1, 2) must (contain (77) or contain (2))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        HashSet(1, 2) must { not { contain (5) } and not { contain (3) }}
        HashSet(1, 2) must ((not contain (5)) and (not contain (3)))
        HashSet(1, 2) must (not contain (5) and not contain (3))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        HashSet(1, 2) must { not { contain (1) } or not { contain (3) }}
        HashSet(1, 2) must ((not contain (1)) or (not contain (3)))
        HashSet(1, 2) must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          HashSet(1, 2) must contain (3)
        }
        assert(caught.getMessage === "Set(1, 2) did not contain element 3")
      }

      it("must throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) must not contain (2)
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) must not (contain (2))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          HashSet(1, 2) must (not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 5")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          HashSet(1, 2) must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          HashSet(1, 2) must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")
      }
    }

    describe("on scala.collection.mutable.HashSet") {

      import scala.collection.mutable

      it("must do nothing if set contains the specified element") {
        mutable.HashSet(1, 2) must contain (2)
        mutable.HashSet(1, 2) must (contain (2))
      }

      it("must do nothing if set does not contain the element and used with must not") {
        mutable.HashSet(1, 2) must not { contain (3) }
        mutable.HashSet(1, 2) must not contain (3)
      }

      it("must do nothing when set contains the specified element and used in a logical-and expression") {
        mutable.HashSet(1, 2) must { contain (2) and (contain (1)) }
        mutable.HashSet(1, 2) must ((contain (2)) and (contain (1)))
        mutable.HashSet(1, 2) must (contain (2) and contain (1))
       }

      it("must do nothing when set contains the specified element and used in a logical-or expression") {
        mutable.HashSet(1, 2) must { contain (77) or (contain (2)) }
        mutable.HashSet(1, 2) must ((contain (77)) or (contain (2)))
        mutable.HashSet(1, 2) must (contain (77) or contain (2))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        mutable.HashSet(1, 2) must { not { contain (5) } and not { contain (3) }}
        mutable.HashSet(1, 2) must ((not contain (5)) and (not contain (3)))
        mutable.HashSet(1, 2) must (not contain (5) and not contain (3))
      }

      it("must do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        mutable.HashSet(1, 2) must { not { contain (1) } or not { contain (3) }}
        mutable.HashSet(1, 2) must ((not contain (1)) or (not contain (3)))
        mutable.HashSet(1, 2) must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must contain (3)
        }
        assert(caught.getMessage === "Set(1, 2) did not contain element 3")
      }

      it("must throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must not contain (2)
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must not (contain (2))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must (not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 5")
      }

      it("must throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          mutable.HashSet(1, 2) must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")
      }
    }

    describe("on List") {

      it("must do nothing if list contains the specified element") {
        List(1, 2) must contain (2)
        List(1, 2) must (contain (2))
        check((list: List[Int]) => list.size != 0 ==> returnsNormally(list must contain (list(list.length - 1))))
      }

      it("must do nothing if list does not contain the element and used with must not") {
        List(1, 2) must not { contain (3) }
        List(1, 2) must not contain (3)
        check((list: List[Int], i: Int) => !list.exists(_ == i) ==> returnsNormally(list must not { contain (i) }))
        check((list: List[Int], i: Int) => !list.exists(_ == i) ==> returnsNormally(list must not contain (i)))
      }

      it("must do nothing when list contains the specified element and used in a logical-and expression") {
        List(1, 2) must { contain (2) and (contain (1)) }
        List(1, 2) must ((contain (2)) and (contain (1)))
        List(1, 2) must (contain (2) and contain (1))
       }

      it("must do nothing when list contains the specified element and used in a logical-or expression") {
        List(1, 2) must { contain (77) or (contain (2)) }
        List(1, 2) must ((contain (77)) or (contain (2)))
        List(1, 2) must (contain (77) or contain (2))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        List(1, 2) must { not { contain (5) } and not { contain (3) }}
        List(1, 2) must ((not contain (5)) and (not contain (3)))
        List(1, 2) must (not contain (5) and not contain (3))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        List(1, 2) must { not { contain (1) } or not { contain (3) }}
        List(1, 2) must ((not contain (1)) or (not contain (3)))
        List(1, 2) must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          List(1, 2) must contain (3)
        }
        assert(caught.getMessage === "List(1, 2) did not contain element 3")
        check((list: List[String], s: String) => !list.exists(_ == s) ==> throwsTestFailedException(list must contain (s)))
      }

      it("must throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) must not contain (2)
        }
        assert(caught1.getMessage === "List(1, 2) contained element 2")
        check((list: List[String]) => list.length > 0 ==> throwsTestFailedException(list must not contain (list(0))))

        val caught2 = intercept[TestFailedException] {
          List(1, 2) must not (contain (2))
        }
        assert(caught2.getMessage === "List(1, 2) contained element 2")
        check((list: List[String]) => list.length > 0 ==> throwsTestFailedException(list must not (contain (list(0)))))

        val caught3 = intercept[TestFailedException] {
          List(1, 2) must (not contain (2))
        }
        assert(caught3.getMessage === "List(1, 2) contained element 2")
        check((list: List[String]) => list.length > 0 ==> throwsTestFailedException(list must not (contain (list(0)))))
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "List(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "List(1, 2) did not contain element 5")
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "List(1, 2) did not contain element 55, and List(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "List(1, 2) did not contain element 55, and List(1, 2) did not contain element 22")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "List(1, 2) did not contain element 3, but List(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "List(1, 2) did not contain element 3, but List(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          List(1, 2) must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "List(1, 2) did not contain element 3, but List(1, 2) contained element 2")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "List(1, 2) contained element 2, and List(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "List(1, 2) contained element 2, and List(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          List(1, 2) must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "List(1, 2) contained element 2, and List(1, 2) contained element 2")
      }
    }

    describe("on java.util.List") {

      val javaList: java.util.List[Int] = new java.util.ArrayList
      javaList.add(1)
      javaList.add(2)
      
      it("must do nothing if list contains the specified element") {
        javaList must contain (2)
        javaList must (contain (2))
      }

      it("must do nothing if list does not contain the element and used with must not") {
        javaList must (not contain (3))
        javaList must not { contain (3) }
        javaList must not contain (3)
      }

      it("must do nothing when list contains the specified element and used in a logical-and expression") {
        javaList must { contain (2) and (contain (1)) }
        javaList must ((contain (2)) and (contain (1)))
        javaList must (contain (2) and contain (1))
       }

      it("must do nothing when list contains the specified element and used in a logical-or expression") {
        javaList must { contain (77) or (contain (2)) }
        javaList must ((contain (77)) or (contain (2)))
        javaList must (contain (77) or contain (2))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        javaList must { not { contain (5) } and not { contain (3) }}
        javaList must ((not contain (5)) and (not contain (3)))
        javaList must (not contain (5) and not contain (3))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        javaList must { not { contain (1) } or not { contain (3) }}
        javaList must ((not contain (1)) or (not contain (3)))
        javaList must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          javaList must contain (3)
        }
        assert(caught.getMessage === "[1, 2] did not contain element 3")
      }

      it("must throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          javaList must not contain (2)
        }
        assert(caught1.getMessage === "[1, 2] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaList must not (contain (2))
        }
        assert(caught2.getMessage === "[1, 2] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaList must (not contain (2))
        }
        assert(caught3.getMessage === "[1, 2] contained element 2")
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          javaList must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "[1, 2] did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaList must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "[1, 2] did not contain element 5")
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          javaList must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "[1, 2] did not contain element 55, and [1, 2] did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          javaList must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "[1, 2] did not contain element 55, and [1, 2] did not contain element 22")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaList must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "[1, 2] did not contain element 3, but [1, 2] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaList must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "[1, 2] did not contain element 3, but [1, 2] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaList must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "[1, 2] did not contain element 3, but [1, 2] contained element 2")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaList must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "[1, 2] contained element 2, and [1, 2] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaList must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "[1, 2] contained element 2, and [1, 2] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaList must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "[1, 2] contained element 2, and [1, 2] contained element 2")
      }
    }

    describe("on scala.collection.immutable.Map") {

      it("must do nothing if map contains specified element") {
        Map("one" -> 1, "two" -> 2) must contain ("two" -> 2)
        Map("one" -> 1, "two" -> 2) must (contain ("two" -> 2))
        Map(1 -> "one", 2 -> "two") must contain (2 -> "two")
      }

      it("must do nothing if map does not contain the specified element and used with not") {
        Map("one" -> 1, "two" -> 2) must not { contain ("three" -> 3) }
        Map("one" -> 1, "two" -> 2) must not contain ("three" -> 3)
        Map("one" -> 1, "two" -> 2) must (not contain ("three" -> 3))
      }

      it("must do nothing when map contains specified element and used in a logical-and expression") {
        Map("one" -> 1, "two" -> 2) must { contain ("two" -> 2) and (contain ("one" -> 1)) }
        Map("one" -> 1, "two" -> 2) must ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        Map("one" -> 1, "two" -> 2) must (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("must do nothing when map contains specified element and used in a logical-or expression") {
        Map("one" -> 1, "two" -> 2) must { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        Map("one" -> 1, "two" -> 2) must ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        Map("one" -> 1, "two" -> 2) must (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        Map("one" -> 1, "two" -> 2) must { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        Map("one" -> 1, "two" -> 2) must ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        Map("one" -> 1, "two" -> 2) must (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        Map("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        Map("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        Map("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("must throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3)")
      }

      it("must throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
      }
    }

    describe("on scala.collection.mutable.Map") {

      import scala.collection.mutable

      it("must do nothing if map contains specified element") {
        mutable.Map("one" -> 1, "two" -> 2) must contain ("two" -> 2)
        mutable.Map("one" -> 1, "two" -> 2) must (contain ("two" -> 2))
        mutable.Map(1 -> "one", 2 -> "two") must contain (2 -> "two")
      }

      it("must do nothing if map does not contain the specified element and used with not") {
        mutable.Map("one" -> 1, "two" -> 2) must not { contain ("three" -> 3) }
        mutable.Map("one" -> 1, "two" -> 2) must not contain ("three" -> 3)
        mutable.Map("one" -> 1, "two" -> 2) must (not contain ("three" -> 3))
      }

      it("must do nothing when map contains specified element and used in a logical-and expression") {
        mutable.Map("one" -> 1, "two" -> 2) must { contain ("two" -> 2) and (contain ("one" -> 1)) }
        mutable.Map("one" -> 1, "two" -> 2) must ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        mutable.Map("one" -> 1, "two" -> 2) must (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("must do nothing when map contains specified element and used in a logical-or expression") {
        mutable.Map("one" -> 1, "two" -> 2) must { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        mutable.Map("one" -> 1, "two" -> 2) must ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        mutable.Map("one" -> 1, "two" -> 2) must (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        mutable.Map("one" -> 1, "two" -> 2) must { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        mutable.Map("one" -> 1, "two" -> 2) must ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        mutable.Map("one" -> 1, "two" -> 2) must (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        mutable.Map("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        mutable.Map("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        mutable.Map("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("must throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3)")
      }

      it("must throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
      }
    }

    describe("on scala.collection.Map") {

      val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

      it("must do nothing if map contains specified element") {
        map must contain ("two" -> 2)
        map must (contain ("two" -> 2))
        map must contain ("two" -> 2)
      }

      it("must do nothing if map does not contain the specified element and used with not") {
        map must not { contain ("three" -> 3) }
        map must not contain ("three" -> 3)
        map must (not contain ("three" -> 3))
      }

      it("must do nothing when map contains specified element and used in a logical-and expression") {
        map must { contain ("two" -> 2) and (contain ("one" -> 1)) }
        map must ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        map must (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("must do nothing when map contains specified element and used in a logical-or expression") {
        map must { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        map must ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        map must (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        map must { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        map must ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        map must (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        map must { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        map must ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        map must (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("must throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          map must contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3)")
      }

      it("must throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          map must (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map must not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map must not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          map must { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          map must ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          map must (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          map must { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          map must ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          map must (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          map must { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map must ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map must (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          map must { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map must ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map must (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
      }
    }

    describe("on scala.collection.immutable.HashMap") {

      import scala.collection.immutable.HashMap

      it("must do nothing if map contains specified element") {
        HashMap("one" -> 1, "two" -> 2) must contain ("two" -> 2)
        HashMap("one" -> 1, "two" -> 2) must (contain ("two" -> 2))
        HashMap(1 -> "one", 2 -> "two") must contain (2 -> "two")
      }

      it("must do nothing if map does not contain the specified element and used with not") {
        HashMap("one" -> 1, "two" -> 2) must not { contain ("three" -> 3) }
        HashMap("one" -> 1, "two" -> 2) must not contain ("three" -> 3)
        HashMap("one" -> 1, "two" -> 2) must (not contain ("three" -> 3))
      }

      it("must do nothing when map contains specified element and used in a logical-and expression") {
        HashMap("one" -> 1, "two" -> 2) must { contain ("two" -> 2) and (contain ("one" -> 1)) }
        HashMap("one" -> 1, "two" -> 2) must ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        HashMap("one" -> 1, "two" -> 2) must (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("must do nothing when map contains specified element and used in a logical-or expression") {
        HashMap("one" -> 1, "two" -> 2) must { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        HashMap("one" -> 1, "two" -> 2) must ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        HashMap("one" -> 1, "two" -> 2) must (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        HashMap("one" -> 1, "two" -> 2) must { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        HashMap("one" -> 1, "two" -> 2) must ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        HashMap("one" -> 1, "two" -> 2) must (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        HashMap("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        HashMap("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        HashMap("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("must throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3)")
      }

      it("must throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
      }
    }

    describe("on scala.collection.mutable.HashMap") {

      import scala.collection.mutable

      it("must do nothing if map contains specified element") {
        mutable.HashMap("one" -> 1, "two" -> 2) must contain ("two" -> 2)
        mutable.HashMap("one" -> 1, "two" -> 2) must (contain ("two" -> 2))
        mutable.HashMap(1 -> "one", 2 -> "two") must contain (2 -> "two")
      }

      it("must do nothing if map does not contain the specified element and used with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) must not { contain ("three" -> 3) }
        mutable.HashMap("one" -> 1, "two" -> 2) must not contain ("three" -> 3)
        mutable.HashMap("one" -> 1, "two" -> 2) must (not contain ("three" -> 3))
      }

      it("must do nothing when map contains specified element and used in a logical-and expression") {
        mutable.HashMap("one" -> 1, "two" -> 2) must { contain ("two" -> 2) and (contain ("one" -> 1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) must ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        mutable.HashMap("one" -> 1, "two" -> 2) must (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("must do nothing when map contains specified element and used in a logical-or expression") {
        mutable.HashMap("one" -> 1, "two" -> 2) must { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) must ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        mutable.HashMap("one" -> 1, "two" -> 2) must (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) must { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) must ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        mutable.HashMap("one" -> 1, "two" -> 2) must (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("must do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        mutable.HashMap("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("must throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3)")
      }

      it("must throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (five,5)")
      }

      it("must throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (fifty five,55), and Map(one -> 1, two -> 2) did not contain element (twenty two,22)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain element (three,3), but Map(one -> 1, two -> 2) contained element (two,2)")
      }

      it("must throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) must (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained element (two,2), and Map(one -> 1, two -> 2) contained element (two,2)")
      }
    }

    describe("on java.util.Set") {

      val javaSet: java.util.Set[Int] = new java.util.HashSet
      javaSet.add(1)
      javaSet.add(2)

      it("must do nothing if list contains the specified element") {
        javaSet must contain (2)
        javaSet must (contain (2))
      }

      it("must do nothing if list does not contain the element and used with must not") {
        javaSet must (not contain (3))
        javaSet must not { contain (3) }
        javaSet must not contain (3)
      }

      it("must do nothing when list contains the specified element and used in a logical-and expression") {
        javaSet must { contain (2) and (contain (1)) }
        javaSet must ((contain (2)) and (contain (1)))
        javaSet must (contain (2) and contain (1))
       }

      it("must do nothing when list contains the specified element and used in a logical-or expression") {
        javaSet must { contain (77) or (contain (2)) }
        javaSet must ((contain (77)) or (contain (2)))
        javaSet must (contain (77) or contain (2))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        javaSet must { not { contain (5) } and not { contain (3) }}
        javaSet must ((not contain (5)) and (not contain (3)))
        javaSet must (not contain (5) and not contain (3))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        javaSet must { not { contain (1) } or not { contain (3) }}
        javaSet must ((not contain (1)) or (not contain (3)))
        javaSet must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          javaSet must contain (3)
        }
        assert(caught.getMessage === "[2, 1] did not contain element 3")
      }

      it("must throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          javaSet must not contain (2)
        }
        assert(caught1.getMessage === "[2, 1] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet must not (contain (2))
        }
        assert(caught2.getMessage === "[2, 1] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet must (not contain (2))
        }
        assert(caught3.getMessage === "[2, 1] contained element 2")
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          javaSet must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "[2, 1] did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaSet must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "[2, 1] did not contain element 5")
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          javaSet must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "[2, 1] did not contain element 55, and [2, 1] did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          javaSet must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "[2, 1] did not contain element 55, and [2, 1] did not contain element 22")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaSet must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaSet must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")
      }
    }

/*
    I'm just not going to support this for now. Let them do whatever, and when someone
    comes back with a good suggestion, then I can consider adding it.

    describe("on java.util.Map") {

      val javaMap: java.util.Map[String, Int] = new java.util.HashMap
      javaMap.put("one",1)
      javaMap.put("two", 2)

      import java.util.Map.Entry

      it("must do nothing if list contains the specified element") {
        javaMap.entrySet must contain ("one" -> 1)
        javaMap.entrySet must (contain ("two" -> 2))
      }

      it("must do nothing if list does not contain the element and used with must not") {
        javaMap must (not contain (3))
        javaMap must not { contain (3) }
        javaMap must not contain (3)
      }

      it("must do nothing when list contains the specified element and used in a logical-and expression") {
        javaMap must { contain (2) and (contain (1)) }
        javaMap must ((contain (2)) and (contain (1)))
        javaMap must (contain (2) and contain (1))
       }

      it("must do nothing when list contains the specified element and used in a logical-or expression") {
        javaMap must { contain (77) or (contain (2)) }
        javaMap must ((contain (77)) or (contain (2)))
        javaMap must (contain (77) or contain (2))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        javaMap must { not { contain (5) } and not { contain (3) }}
        javaMap must ((not contain (5)) and (not contain (3)))
        javaMap must (not contain (5) and not contain (3))
      }

      it("must do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        javaMap must { not { contain (1) } or not { contain (3) }}
        javaMap must ((not contain (1)) or (not contain (3)))
        javaMap must (not contain (3) or not contain (2))
      }

      it("must throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          javaMap must contain (3)
        }
        assert(caught.getMessage === "{one=1, two=2} did not contain element 3")
      }

      it("must throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap must not contain (2)
        }
        assert(caught1.getMessage === "{one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap must not (contain (2))
        }
        assert(caught2.getMessage === "{one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap must (not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} contained element 2")
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          javaMap must { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaMap must (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 5")
      }

      it("must throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          javaMap must { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 55, and {one=1, two=2} did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          javaMap must (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 55, and {one=1, two=2} did not contain element 22")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap must { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap must ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap must (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")
      }

      it("must throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap must { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap must ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap must (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")
      }
    }
*/
  }
}
