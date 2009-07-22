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

/*
This has a bit of redundancy with several other specs, but was the
original file I used to develop the matchers syntax, and it has a few
tests that don't exist elsewhere, so I'm keeping it alive for now.
*/
class MustMatcherSpec extends Spec with MustMatchers {

  describe("The be matcher") {

    describe("(for booleans)") {

      it("must do nothing when false is compared to false") {
        false must be (false)
      }

      it("must do nothing when true is compared to true") {
        true must be (true)
      }

      it("must throw an assertion error when not equal") {
        val caught = intercept[TestFailedException] {
          false must be (true)
        }
        assert(caught.getMessage === "false was not true")
      }
    }

    describe("(for null)") {

      it("must do nothing when null is compared to null") {
        val o: String = null
        o must be (null)
        o must equal (null)
      }

      it("must throw an assertion error when non-null compared to null") {
        val caught = intercept[TestFailedException] {
          val o = "Helloooooo"
          o must be (null)
        }
        assert(caught.getMessage === "\"Helloooooo\" was not null")
      }

      it("must do nothing when non-null is compared to not null") {
        val o = "Helloooooo"
        o must not { be (null) }
      }

      it("must throw an assertion error when null compared to not null") {
        val caught1 = intercept[TestFailedException] {
          val o: String = null
          o must not { be (null) }
        }
        assert(caught1.getMessage === "The reference was null")
      }

      it("must work when used in a logical expression") {
        val o: String = null
        o must { be (null) and equal (null) }
        o must { equal (null) and be (null) }
      }
    }

    describe("(for Nil)") {

      it("must do nothing when an empty list is compared to Nil") {
        val emptyList = List[String]()
        emptyList must be (Nil)
        emptyList must equal (Nil)
      }

      it("must throw an assertion error when a non-empty list is compared to Nil") {
        val nonEmptyList = List("Helloooooo")
        val caught1 = intercept[TestFailedException] {
          nonEmptyList must be (Nil)
        }
        assert(caught1.getMessage === "List(Helloooooo) was not equal to List()")
        val caught2 = intercept[TestFailedException] {
          nonEmptyList must equal (Nil)
        }
        assert(caught2.getMessage === "List(Helloooooo) did not equal List()")
      }

      it("must do nothing when non-null is compared to not null") {
        val nonEmptyList = List("Helloooooo")
        nonEmptyList must not { be (Nil) }
        nonEmptyList must not { equal (Nil) }
      }

      it("must throw an assertion error when null compared to not null") {
        val emptyList = List[String]()
        val caught1 = intercept[TestFailedException] {
          emptyList must not { be (Nil) }
        }
        assert(caught1.getMessage === "List() was equal to List()")

        val caught3 = intercept[TestFailedException] {
          emptyList must not { equal (Nil) }
        }
        assert(caught3.getMessage === "List() equaled List()")
      }

      it("must work when used in a logical expression") {
        val emptyList = List[Int]()
        emptyList must { be (Nil) and equal (Nil) }
        emptyList must { equal (Nil) and be (Nil) } // Nada, and nada is nada
      }
    }

    describe("(for None)") {

        /* I think I must have tests for options somewhere
        val option = Some(1)
        option must equal (Some(1))
      val option = Some(1)
      option must not { equal (Some(2)) }

         */
      it("must do nothing when a None option is compared to None") {
        val option: Option[String] = None
        option must be (None)
        option must equal (None)
      }

      it("must throw an assertion error when a Some is compared to None") {
        val someString = Some("Helloooooo")
        val caught1 = intercept[TestFailedException] {
          someString must be (None)
        }
        assert(caught1.getMessage === "Some(Helloooooo) was not equal to None")
        val caught2 = intercept[TestFailedException] {
          someString must equal (None)
        }
        assert(caught2.getMessage === "Some(Helloooooo) did not equal None")
      }

      it("must do nothing when Some is compared to not None") {
        val someString = Some("Helloooooo")
        someString must not { be (None) }
        someString must not { equal (None) }
      }

      it("must throw an assertion error when None compared to not None") {
        val none = None
        val caught1 = intercept[TestFailedException] {
          none must not { be (None) }
        }
        assert(caught1.getMessage === "None was equal to None")

        val caught3 = intercept[TestFailedException] {
          none must not { equal (None) }
        }
        assert(caught3.getMessage === "None equaled None")

        val noString: Option[String] = None
        val caught5 = intercept[TestFailedException] {
          noString must not { be (None) }
        }
        assert(caught5.getMessage === "None was equal to None")

        val caught7 = intercept[TestFailedException] {
          noString must not { equal (None) }
        }
        assert(caught7.getMessage === "None equaled None")
      }

      it("must work when used in a logical expression") {
        val none = None
        none must { be (None) and equal (None) }
        none must { equal (None) and be (None) }
        val noString: Option[String] = None
        noString must { be (None) and equal (None) }
        noString must { equal (None) and be (None) }
      }
    }

    describe("(for Any)") {
      it("must do nothing when equal") {
        1 must be (1)
        val option = Some(1)
        option must be (Some(1)) 
      }

      it("must throw an assertion error when not equal") {
        val caught = intercept[TestFailedException] {
          1 must be (2)
        }
        assert(caught.getMessage === "1 was not equal to 2")
      }

      it("must do nothing when not equal and used with must not") {
        1 must not { be (2) }
        val option = Some(1)
        option must not { be (Some(2)) }
      }

      it("must throw an assertion error when equal but used with must not") {
        val caught = intercept[TestFailedException] {
          1 must not { be (1) }
        }
        assert(caught.getMessage === "1 was equal to 1")
      }
    }
  }

  describe("The have word") {

    it("must work with map and key, right after a 'must'") {
      val map = Map(1 -> "Howdy")
      map must contain key (1)
      map must contain key (1)
      map must equal { Map(1 -> "Howdy") }
      val otherMap = Map("Howdy" -> 1)
      otherMap must contain key ("Howdy")
      otherMap must equal { Map("Howdy" -> 1) }
      import scala.collection.immutable.TreeMap
      val treeMap = TreeMap(1 -> "hi", 2 -> "howdy")
      treeMap must contain key (1)
    }

    it("must work with map and key, in a logical expression") {
      val map = Map(1 -> "Howdy")
      // The compiler infer the type of the value to be Nothing if I say: map must { contain key 1 and equal (Map(1 -> "Howdy")) }
      // map must { have.key[Int, String](1) and equal (Map(1 -> "Howdy")) }
      map must { contain key (1) and equal (Map(1 -> "Howdy")) }
      val otherMap = Map("Howdy" -> 1)
      // otherMap must { have.key[String, Int]("Howdy") and equal (Map("Howdy" -> 1)) }
      otherMap must { contain key ("Howdy") and equal (Map("Howdy" -> 1)) }
    }

    it("must work with map and key, right after a 'must not'") {
      val map = Map(1 -> "Howdy")
      map must not { contain key (2) }
    }

    it("must work with map and value, right after a 'must'") {
      val map = Map(1 -> "Howdy")
      map must contain value ("Howdy")
      map must contain value ("Howdy")
      map must equal { Map(1 -> "Howdy") }
      val otherMap = Map("Howdy" -> 1)
      otherMap must contain value (1)
      otherMap must equal { Map("Howdy" -> 1) }
    }

    it("must work with map and value, in a logical expression") {
      val map = Map(1 -> "Howdy")
      map must { equal (Map(1 -> "Howdy")) and (contain value "Howdy") }
      val otherMap = Map("Howdy" -> 1)
      otherMap must { contain value (1) and equal (Map("Howdy" -> 1)) }
    }

    it("must work with map and value, right after a 'must not'") {
      val map = Map(1 -> "Howdy")
      map must not { contain value ("Doody") }
    }

    it("must work with collection and size, in an and expression.") {
      val list = List(1, 2, 3)
      list must { have size (3) and equal (List(1, 2, 3)) }
    }

    it("must work with collection and size, right after a 'must'") {

      val map = Map(1 -> "Howdy")
      map must have size (1)
      val caught1 = intercept[TestFailedException] {
        map must have size (5)
      }
      assert(caught1.getMessage.indexOf("did not have size") != -1)

      val list = List(1, 2, 3, 4, 5)
      list must have size (5)
      val caught2 = intercept[TestFailedException] {
        list must have size (6)
      }
      assert(caught2.getMessage.indexOf("did not have size") != -1)

      val set = Set(1.0, 2.0, 3.0)
      set must have size (3)
      val caught3 = intercept[TestFailedException] {
        set must have size (0)
      }
      assert(caught3.getMessage.indexOf("did not have size") != -1)

      val array = Array[String]()
      array must have size 0
      val caught4 = intercept[TestFailedException] {
        array must have size 2
      }
      assert(caught4.getMessage.indexOf("did not have size") != -1)
    }

    it("must work with collection and size, right after a 'must not'") {

      val map = Map(1 -> "Howdy")
      map must not { have size (2) }
      val caught1 = intercept[TestFailedException] {
        map must not { have size (1) }
      }
      assert(caught1.getMessage.indexOf("had size") != -1, caught1.getMessage)

      val list = List(1, 2, 3, 4, 5)
      list must not { have size (6) }
      val caught2 = intercept[TestFailedException] {
        list must not { have size (5) }
      }
      assert(caught2.getMessage.indexOf("had size") != -1)

      val set = Set(1.0, 2.0, 3.0)
      set must not { have size (0) }
      val caught3 = intercept[TestFailedException] {
        set must not { have size (3) }
      }
      assert(caught3.getMessage.indexOf("had size") != -1)

      val array = Array[String]()
      array must not { have size (2) }
      val caught4 = intercept[TestFailedException] {
        array must not { have size (0) }
      }
      assert(caught4.getMessage.indexOf("had size") != -1)
    }
  }

  describe("The contain word") {
 
    it("must work with a set, list, array, and map right after a 'must'") {

      val set = Set(1, 2, 3)
      set must contain (2)
      val caught1 = intercept[TestFailedException] {
        set must contain (5)
      }
      assert(caught1.getMessage.indexOf("did not contain element") != -1)

      set must { contain (2) and equal (Set(1, 2, 3)) }
      val caught1b = intercept[TestFailedException] {
        set must { contain (5) and equal(Set(1, 2, 3)) }
      }
      assert(caught1b.getMessage.indexOf("did not contain element") != -1)

      val list = List("one", "two", "three")
      list must contain ("two")
      val caught2 = intercept[TestFailedException] {
        list must contain ("five")
      }
      assert(caught2.getMessage.indexOf("did not contain element") != -1)

      val array = Array("one", "two", "three")
      array must contain ("one")
      val caught3 = intercept[TestFailedException] {
        array must contain ("five")
      }
      assert(caught3.getMessage.indexOf("did not contain element") != -1)

      val map = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val tuple2: Tuple2[Int, String] = 1 -> "one"
      map must contain (tuple2)
      val caught4 = intercept[TestFailedException] {
        map must contain (1 -> "won")
      }
      assert(caught4.getMessage.indexOf("did not contain element") != -1)
    }

    it("must work with a set, list, array, and map right after a 'must not'") {

      val set = Set(1, 2, 3)
      set must not { contain (5) }
      val caught1 = intercept[TestFailedException] {
        set must not { contain (2) }
      }
      assert(caught1.getMessage.indexOf("contained element") != -1)

      val list = List("one", "two", "three")
      list must not { contain ("five") }
      val caught2 = intercept[TestFailedException] {
        list must not { contain ("two") }
      }
      assert(caught2.getMessage.indexOf("contained element") != -1)

      val array = Array("one", "two", "three")
      array must not { contain ("five") }
      val caught3 = intercept[TestFailedException] {
        array must not { contain ("one") }
      }
      assert(caught3.getMessage.indexOf("contained element") != -1)

      val map = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val tuple2: Tuple2[Int, String] = 1 -> "won"
      map must not { contain (tuple2) }
      val caught4 = intercept[TestFailedException] {
        map must not { contain (1 -> "one") }
      }
      assert(caught4.getMessage.indexOf("contained element") != -1)
    }
  }

  describe("The be theSameInstanceAs syntax") {

    val string = "Hi"
    val obj: AnyRef = string
    val otherString = new String("Hi")

    it("must do nothing if the two objects are the same") {
      string must be theSameInstanceAs (string)
      obj must be theSameInstanceAs (string)
      string must be theSameInstanceAs (obj)
      otherString must not { be theSameInstanceAs (string) }
    }

    it("must throw TestFailedException if the two objects are not the same") {
      val caught1 = intercept[TestFailedException] {
        string must not { be theSameInstanceAs (string) }
      }
      val caught2 = intercept[TestFailedException] {
        obj must not { be theSameInstanceAs (string) }
      }
      val caught3 = intercept[TestFailedException] {
        string must not { be theSameInstanceAs (obj) }
      }
      val caught4 = intercept[TestFailedException] {
        otherString must be theSameInstanceAs (string)
      }
      assert(true) // TODO: test the failure message
    }
  }

  describe("The floating point numbers when compared with equals") {
    it("must do nothing if the floating point number is exactly equal to the specified value") {
      val sevenDotOh = 7.0
      sevenDotOh must be (7.0)
      sevenDotOh must equal (7.0)
      sevenDotOh must not { be (7.0001) }

      val sixDotOh: Float = 6.0f
      sixDotOh must be (6.0)
      sixDotOh must equal (6.0)
      sixDotOh must not { be (6.0001) }
    }

    it("must throw TestFailedException if the floating point number is not exactly equal to the specified value") {
      val sevenDotOh = 7.0001
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must be (7.0)
        // sevenDotOh must be (7.0 exactly)
      }
      assert(caught1.getMessage === "7.0001 was not equal to 7.0")

      val caught2 = intercept[TestFailedException] {
        sevenDotOh must equal (7.0)
      }
      assert(caught2.getMessage === "7.0001 did not equal 7.0")

      val caught3 = intercept[TestFailedException] {
        sevenDotOh must not { be (7.0001) }
      }
      assert(caught3.getMessage === "7.0001 was equal to 7.0001")

      val sixDotOh: Float = 6.0001f
      val caught4 = intercept[TestFailedException] {
        // sixDotOh must be (6.0f exactly)
        sixDotOh must be (6.0f)
      }
      assert(caught4.getMessage === "6.0001 was not equal to 6.0")

      val caught5 = intercept[TestFailedException] {
        sixDotOh must equal (6.0f)
      }
      assert(caught5.getMessage === "6.0001 did not equal 6.0")

      val caught6 = intercept[TestFailedException] {
        sixDotOh must not { be (6.0001f) }
      }
      assert(caught6.getMessage === "6.0001 was equal to 6.0001")
    }
  }

  describe("The floating point 'plusOrMinus' operator") {
    it("must do nothing if the floating point number is within the specified range") {
      val sevenDotOh = 7.0
      sevenDotOh must be (7.1 plusOrMinus 0.2)
      sevenDotOh must be (6.9 plusOrMinus 0.2)
      sevenDotOh must not { be (7.5 plusOrMinus 0.2) }
      sevenDotOh must not { be (6.5 plusOrMinus 0.2) }
      val minusSevenDotOh = -7.0
      minusSevenDotOh must be (-7.1 plusOrMinus 0.2)
      minusSevenDotOh must be (-6.9 plusOrMinus 0.2)
      minusSevenDotOh must not { be (-7.5 plusOrMinus 0.2) }
      minusSevenDotOh must not { be (-6.5 plusOrMinus 0.2) }
    }

    it("must throw TestFailedException if the floating point number is not within the specified range") {
      val sevenDotOh = 7.0
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must not { be (7.1 plusOrMinus 0.2) }
      }
      assert(caught1.getMessage === "7.0 was 7.1 plus or minus 0.2")

      val caught2 = intercept[TestFailedException] {
        sevenDotOh must not { be (6.9 plusOrMinus 0.2) }
      }
      assert(caught2.getMessage === "7.0 was 6.9 plus or minus 0.2")

      val caught3 = intercept[TestFailedException] {
        sevenDotOh must be (7.5 plusOrMinus 0.2)
      }
      assert(caught3.getMessage === "7.0 was not 7.5 plus or minus 0.2")

      val caught4 = intercept[TestFailedException] {
        sevenDotOh must be (6.5 plusOrMinus 0.2)
      }
      assert(caught4.getMessage === "7.0 was not 6.5 plus or minus 0.2")

      val minusSevenDotOh = -7.0
      val caught5 = intercept[TestFailedException] {
        minusSevenDotOh must not { be (-7.1 plusOrMinus 0.2) }
      }
      assert(caught5.getMessage === "-7.0 was -7.1 plus or minus 0.2")

      val caught6 = intercept[TestFailedException] {
        minusSevenDotOh must not { be (-6.9 plusOrMinus 0.2) }
      }
      assert(caught6.getMessage === "-7.0 was -6.9 plus or minus 0.2")

      val caught7 = intercept[TestFailedException] {
        minusSevenDotOh must be (-7.5 plusOrMinus 0.2)
      }
      assert(caught7.getMessage === "-7.0 was not -7.5 plus or minus 0.2")

      val caught8 = intercept[TestFailedException] {
        minusSevenDotOh must be (-6.5 plusOrMinus 0.2)
      }
      assert(caught8.getMessage === "-7.0 was not -6.5 plus or minus 0.2")
    }
  }
}
