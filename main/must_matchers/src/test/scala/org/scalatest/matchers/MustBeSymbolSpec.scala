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

class MustBeSymbolSpec extends Spec with MustMatchers with EmptyMocks {

  describe("The be ('symbol) syntax") {

    it("must do nothing if the object has an appropriately named method, which returns true") {
      emptyMock must be ('empty)
      isEmptyMock must be ('empty)
    }

    it("must throw TestFailedException if no <symbol> or is<Symbol> method exists") {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock must be ('empty)
      }
      ex1.getMessage must equal ("NoPredicateMock has neither an empty nor an isEmpty method")
      // Check message for name that starts with a consonant (must use a instead of an)
      val ex2 = intercept[TestFailedException] {
        noPredicateMock must be ('full)
      }
      ex2.getMessage must equal ("NoPredicateMock has neither a full nor an isFull method")
    }

    it("must do nothing if the object has an appropriately named method, which returns true, even if the method contains operator characters") {

      val opNames = new OperatorNames

      opNames must be ('op_21_!)
      opNames must be ('op_23_#)
      opNames must be ('op_25_%)
      opNames must be ('op_26_&)
      opNames must be ('op_2a_*)
      opNames must be ('op_2b_+)
      opNames must be ('op_2d_-)
      opNames must be ('op_2f_/)
      opNames must be ('op_3a_:)
      opNames must be ('op_3c_<)
      opNames must be ('op_3d_=)
      opNames must be ('op_3e_>)
      opNames must be ('op_3f_?)
      opNames must be ('op_40_@)
      opNames must be ('op_5c_\)
      opNames must be ('op_5e_^)
      opNames must be ('op_7c_|)
      opNames must be ('op_7e_~)

      opNames must be (Symbol("!!!"))
      opNames must be (Symbol("###"))
      opNames must be (Symbol("%%%"))
      opNames must be (Symbol("&&&"))
      opNames must be (Symbol("***"))
      opNames must be (Symbol("+++"))
      opNames must be (Symbol("---"))
      opNames must be (Symbol("/"))
      opNames must be (Symbol(":::"))
      opNames must be (Symbol("<<<"))
      opNames must be (Symbol("==="))
      opNames must be (Symbol(">>>"))
      opNames must be (Symbol("???"))
      opNames must be (Symbol("@@@"))
      opNames must be (Symbol("\\\\\\"))
      opNames must be (Symbol("^^^"))
      opNames must be (Symbol("|||"))
      opNames must be (Symbol("~~~"))
    }

    it("must do nothing if the object has an appropriately named method, which returns false when used with not") {
      notEmptyMock must not { be ('empty) }
      notEmptyMock must not be ('empty)
      isNotEmptyMock must not { be ('empty) }
      isNotEmptyMock must not be ('empty)
    }

    it("must throw TestFailedException if no <symbol> or is<Symbol> method exists, when used with not") {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock must not { be ('empty) }
      }
      ex1.getMessage must equal ("NoPredicateMock has neither an empty nor an isEmpty method")
      val ex2 = intercept[TestFailedException] {
        noPredicateMock must not (be ('full))
      }
      ex2.getMessage must equal ("NoPredicateMock has neither a full nor an isFull method")
      val ex3 = intercept[TestFailedException] {
        noPredicateMock must not be ('empty)
      }
      ex3.getMessage must equal ("NoPredicateMock has neither an empty nor an isEmpty method")
      val ex4 = intercept[TestFailedException] {
        noPredicateMock must not be ('full)
      }
      ex4.getMessage must equal ("NoPredicateMock has neither a full nor an isFull method")
    }

    it("must do nothing if the object has an appropriately named method, which returns true, when used in a logical-and expression") {
      emptyMock must ((be ('empty)) and (be ('empty)))
      emptyMock must (be ('empty) and (be ('empty)))
      emptyMock must (be ('empty) and be ('empty))
      isEmptyMock must ((be ('empty)) and (be ('empty)))
      isEmptyMock must (be ('empty) and (be ('empty)))
      isEmptyMock must (be ('empty) and be ('empty))
    }

    it("must do nothing if the object has an appropriately named method, which returns true, when used in a logical-or expression") {

      emptyMock must ((be ('full)) or (be ('empty)))
      emptyMock must (be ('full) or (be ('empty)))
      emptyMock must (be ('full) or be ('empty))
      isEmptyMock must ((be ('full)) or (be ('empty)))
      isEmptyMock must (be ('full) or (be ('empty)))
      isEmptyMock must (be ('full) or be ('empty))

      emptyMock must ((be ('empty)) or (be ('full)))
      emptyMock must (be ('empty) or (be ('full)))
      emptyMock must (be ('empty) or be ('full))
      isEmptyMock must ((be ('empty)) or (be ('full)))
      isEmptyMock must (be ('empty) or (be ('full)))
      isEmptyMock must (be ('empty) or be ('full))
    }

    it("must do nothing if the object has an appropriately named method, which returns false, when used in a logical-and expression with not") {

      notEmptyMock must (not (be ('empty)) and not (be ('empty)))
      notEmptyMock must ((not be ('empty)) and (not be ('empty)))
      notEmptyMock must (not be ('empty) and not be ('empty))

      isNotEmptyMock must (not (be ('empty)) and not (be ('empty)))
      isNotEmptyMock must ((not be ('empty)) and (not be ('empty)))
      isNotEmptyMock must (not be ('empty) and not be ('empty))
    }

    it("must do nothing if the object has an appropriately named method, which returns false, when used in a logical-or expression with not") {

      notEmptyMock must (not (be ('empty)) or not (be ('empty)))
      notEmptyMock must ((not be ('empty)) or (not be ('empty)))
      notEmptyMock must (not be ('empty) or not be ('empty))

      isNotEmptyMock must (not (be ('empty)) or not (be ('empty)))
      isNotEmptyMock must ((not be ('empty)) or (not be ('empty)))
      isNotEmptyMock must (not be ('empty) or not be ('empty))

      notEmptyMock must (not (be ('full)) or not (be ('empty)))
      notEmptyMock must ((not be ('full)) or (not be ('empty)))
      notEmptyMock must (not be ('full) or not be ('empty))

      isNotEmptyMock must (not (be ('full)) or not (be ('empty)))
      isNotEmptyMock must ((not be ('full)) or (not be ('empty)))
      isNotEmptyMock must (not be ('full) or not be ('empty))
    }

    it("must throw TestFailedException if the object has an appropriately named method, which returns false") {
      val caught1 = intercept[TestFailedException] {
        notEmptyMock must be ('empty)
      }
      assert(caught1.getMessage === "NotEmptyMock was not empty")
      val caught2 = intercept[TestFailedException] {
        isNotEmptyMock must be ('empty)
      }
      assert(caught2.getMessage === "IsNotEmptyMock was not empty")
    }

    it("must throw TestFailedException if the object has an appropriately named method, which returns true when used with not") {
      val caught1 = intercept[TestFailedException] {
        emptyMock must not { be ('empty) }
      }
      assert(caught1.getMessage === "EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock must not be ('empty)
      }
      assert(caught2.getMessage === "EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        isEmptyMock must not { be ('empty) }
      }
      assert(caught3.getMessage === "IsEmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock must not be ('empty)
      }
      assert(caught4.getMessage === "IsEmptyMock was empty")
    }

    it("must throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-and expression") {
      val caught1 = intercept[TestFailedException] {
        emptyMock must ((be ('empty)) and (be ('full)))
      }
      assert(caught1.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught2 = intercept[TestFailedException] {
        emptyMock must (be ('empty) and (be ('full)))
      }
      assert(caught2.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught3 = intercept[TestFailedException] {
        emptyMock must (be ('empty) and be ('full))
      }
      assert(caught3.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock must ((be ('empty)) and (be ('full)))
      }
      assert(caught4.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock must (be ('empty) and (be ('full)))
      }
      assert(caught5.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock must (be ('empty) and be ('full))
      }
      assert(caught6.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
    }

    it("must throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-or expression") {

      val caught1 = intercept[TestFailedException] {
        notEmptyMock must ((be ('empty)) or (be ('empty)))
      }
      assert(caught1.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught2 = intercept[TestFailedException] {
        notEmptyMock must (be ('empty) or (be ('empty)))
      }
      assert(caught2.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught3 = intercept[TestFailedException] {
        notEmptyMock must (be ('empty) or be ('empty))
      }
      assert(caught3.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught4 = intercept[TestFailedException] {
        isNotEmptyMock must ((be ('empty)) or (be ('empty)))
      }
      assert(caught4.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
      val caught5 = intercept[TestFailedException] {
        isNotEmptyMock must (be ('empty) or (be ('empty)))
      }
      assert(caught5.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
      val caught6 = intercept[TestFailedException] {
        isNotEmptyMock must (be ('empty) or be ('empty))
      }
      assert(caught6.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
    }

    it("must throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-and expression with not") {

      val caught1 = intercept[TestFailedException] {
        emptyMock must (not (be ('full)) and not (be ('empty)))
      }
      assert(caught1.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock must ((not be ('full)) and (not be ('empty)))
      }
      assert(caught2.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        emptyMock must (not be ('full) and not be ('empty))
      }
      assert(caught3.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock must (not (be ('full)) and not (be ('empty)))
      }
      assert(caught4.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock must ((not be ('full)) and (not be ('empty)))
      }
      assert(caught5.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock must (not be ('full) and not be ('empty))
      }
      assert(caught6.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      // Check that the error message "short circuits"
      val caught7 = intercept[TestFailedException] {
        emptyMock must (not (be ('empty)) and not (be ('full)))
      }
      assert(caught7.getMessage === "EmptyMock was empty")
    }

    it("must throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-or expression with not") {

      val caught1 = intercept[TestFailedException] {
        emptyMock must (not (be ('empty)) or not (be ('empty)))
      }
      assert(caught1.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock must ((not be ('empty)) or (not be ('empty)))
      }
      assert(caught2.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        emptyMock must (not be ('empty) or not be ('empty))
      }
      assert(caught3.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock must (not (be ('empty)) or not (be ('empty)))
      }
      assert(caught4.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock must ((not be ('empty)) or (not be ('empty)))
      }
      assert(caught5.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock must (not be ('empty) or not be ('empty))
      }
      assert(caught6.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
    }

    describe("(for the different types that have implicit conversions for must methods)") {

      // FOR: implicit def convertToCollectionMustWrapper[T](o: Collection[T])...
      it("must work on a scala.Collection") {
        val emptySet = Set[Int]()
        emptySet must be ('empty)
        val nonEmptySet = Set(1, 2, 3)
        nonEmptySet must not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptySet must be ('empty)
        }
        assert(caught1.getMessage === "Set(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptySet must not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "Set(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptySet must not { be ('happy) }
        }
        assert(caught3.getMessage === "Set(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToSeqMustWrapper[T](o: Seq[T])...
      it("must work on a scala.Seq") {
        import scala.collection.mutable.ListBuffer
        val emptyListBuffer = new ListBuffer[Int]
        emptyListBuffer must be ('empty)
        val nonEmptyListBuffer = new ListBuffer[Int]
        nonEmptyListBuffer += 1
        nonEmptyListBuffer += 2
        nonEmptyListBuffer += 3
        nonEmptyListBuffer must not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyListBuffer must be ('empty)
        }
        assert(caught1.getMessage === "ListBuffer(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyListBuffer must not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "ListBuffer(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyListBuffer must not { be ('happy) }
        }
        assert(caught3.getMessage === "ListBuffer(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToArrayMustWrapper[T](o: Array[T]): ArrayMustWrapper[T] = new ArrayMustWrapper[T](o)
      it("must work on a scala.Array") {
        val emptyArray = new Array[Int](0)
        emptyArray must be ('empty)
        val nonEmptyArray = Array(1, 2, 3)
        nonEmptyArray must not be ('empty)
        val caught1 = intercept[TestFailedException] {
          nonEmptyArray must be ('empty)
        }
        assert(caught1.getMessage === "Array(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyArray must not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "Array(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyArray must not { be ('happy) }
        }
        assert(caught3.getMessage === "Array(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToListMustWrapper[T](o: List[T])...
      it("must work on a scala.List") {
        val emptyList = List[Int]()
        emptyList must be ('empty)
        val nonEmptyList = List(1, 2, 3)
        nonEmptyList must not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyList must be ('empty)
        }
        assert(caught1.getMessage === "List(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyList must not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "List(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyList must not { be ('happy) }
        }
        assert(caught3.getMessage === "List(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToMapMustWrapper[K, V](o: scala.collection.Map[K, V])...
      it("must work on a scala.Map") {
        val emptyMap = Map[Int, String]()
        emptyMap must be ('empty)
        val nonEmptyMap = Map("one" -> 1, "two" -> 2, "three" -> 3)
        nonEmptyMap must not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyMap must be ('empty)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2, three -> 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyMap must not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2, three -> 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyMap must not { be ('happy) }
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2, three -> 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToStringMustWrapper[K, V](o: String): StringMustWrapper = new StringMustWrapper(o)
      it("must work on a String") {
        val caught3 = intercept[TestFailedException] {
          "howdy" must not be ('happy)
        }
        assert(caught3.getMessage === "\"howdy\" has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToJavaCollectionMustWrapper[T](o: java.util.Collection[T])...
      it("must work on a java.util.Collection") {
        val emptySet = new java.util.HashSet[Int]
        emptySet must be ('empty)
        val nonEmptySet = new java.util.HashSet[Int]
        nonEmptySet.add(1)
        nonEmptySet.add(2)
        nonEmptySet.add(3)
        nonEmptySet must not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptySet must be ('empty)
        }
        assert(caught1.getMessage === "[2, 1, 3] was not empty")
        val caught3 = intercept[TestFailedException] {
          nonEmptySet must not { be ('happy) }
        }
        assert(caught3.getMessage === "[2, 1, 3] has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToJavaListMustWrapper[T](o: java.util.List[T])...
      it("must work on a java.util.List") {
        val emptyList = new java.util.ArrayList[Int]
        emptyList must be ('empty)
        val nonEmptyList = new java.util.ArrayList[Int]
        nonEmptyList.add(1)
        nonEmptyList.add(2)
        nonEmptyList.add(3)
        nonEmptyList must not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyList must be ('empty)
        }
        assert(caught1.getMessage === "[1, 2, 3] was not empty")
        val caught3 = intercept[TestFailedException] {
          nonEmptyList must not { be ('happy) }
        }
        assert(caught3.getMessage === "[1, 2, 3] has neither a happy nor an isHappy method")
      }
    }
  }

  describe("The be matcher") {

    describe("(for symbols)") {

      // TODO: Make sure to write a test for each conversion, because some are using MustMethodsForAny instead
      // of MustMethodsForAnyRef.
      it("must be invokable from be a Symbol and be an Symbol") {
        val emptySet = Set()
        emptySet must be a ('empty)
        emptySet must be an ('empty)
        val nonEmptySet = Set(1, 2, 3)
        nonEmptySet must not { be a ('empty) }
        nonEmptySet must not { be an ('empty) }
      }

      it("must call empty when passed 'empty") {
        class EmptyMock {
          def empty: Boolean = true
        }
        class NonEmptyMock {
          def empty: Boolean = false
        }
        (new EmptyMock) must be ('empty)
        (new NonEmptyMock) must not { be ('empty) }
        // (new NonEmptyMock) mustNot be ('empty)
      }

// STOLE FROM HERE
      it("must call the Scala=style method if both an empty and an isEmpty method exist") {
        class EmptyMock {
          def empty: Boolean = true
          def isEmpty: Boolean = false
          override def toString = "EmptyMock"
        }
        class NonEmptyMock {
          def empty: Boolean = false
          def isEmpty: Boolean = true
          override def toString = "NonEmptyMock"
        }
        (new EmptyMock) must be ('empty)
        (new NonEmptyMock) must not { be ('empty) }
      }

      it("must access an 'empty' val when passed 'empty") {
        class EmptyMock {
          val empty: Boolean = true
        }
        class NonEmptyMock {
          val empty: Boolean = false
        }
        (new EmptyMock) must be ('empty)
        (new NonEmptyMock) must not { be ('empty) }
      }
    }
  }

  describe("the be ('empty) syntax") {

    it("must call isEmpty") {
      val emptySet = Set[Int]()
      emptySet must be ('empty)
      val nonEmptySet = Set(1, 2, 3)
      nonEmptySet must not { be ('empty) }
    }

    it("must call empty when passed 'empty") {
      class EmptyMock {
        def empty: Boolean = true
      }
      class NonEmptyMock {
        def empty: Boolean = false
      }
      (new EmptyMock) must be ('empty)
      (new NonEmptyMock) must not { be ('empty) }
      // (new NonEmptyMock) mustNot be ('empty)
    }

    it("must throw TestFailedException if no empty or isEmpty method") {
      class EmptyMock {
        override def toString = "EmptyMock"
      }
      class NonEmptyMock {
        override def toString = "NonEmptyMock"
      }
      val ex1 = intercept[TestFailedException] {
        (new EmptyMock) must be ('empty)
      }
      ex1.getMessage must equal ("EmptyMock has neither an empty nor an isEmpty method")
      val ex2 = intercept[TestFailedException] {
        (new NonEmptyMock) must not { be ('empty) }
      }
      ex2.getMessage must equal ("NonEmptyMock has neither an empty nor an isEmpty method")
    }

    it("must call the Scala-style method if both an empty and an isEmpty method exist") {
      class EmptyMock {
        def empty: Boolean = true
        def isEmpty: Boolean = false
        override def toString = "EmptyMock"
      }
      class NonEmptyMock {
        def empty: Boolean = false
        def isEmpty: Boolean = true
        override def toString = "NonEmptyMock"
      }
      (new EmptyMock) must be ('empty)
      (new NonEmptyMock) must not { be ('empty) }
    }

    it("must access an 'empty' val when passed 'empty") {
      class EmptyMock {
        val empty: Boolean = true
      }
      class NonEmptyMock {
        val empty: Boolean = false
      }
      (new EmptyMock) must be ('empty)
      (new NonEmptyMock) must not { be ('empty) }
      // (new NonEmptyMock) mustNot be ('empty)
    }
  }

  describe("The be 'defined syntax") {

    it("must do nothing when used with a Some") {
      val someString: Some[String] = Some("hi")
      someString must be ('defined)
      val optionString: Option[String] = Some("hi")
      optionString must be ('defined)
    }

    it("must throw TestFailedException when used with a None") {
      val none: None.type = None
      val caught1 = intercept[TestFailedException] {
        none must be ('defined)
      }
      assert(caught1.getMessage === "None was not defined")
      val option: Option[Int] = None
      val caught2 = intercept[TestFailedException] {
        option must be ('defined)
      }
      assert(caught2.getMessage === "None was not defined")
    }

    it("must call defined") {
      class DefinedMock {
        def defined: Boolean = true
      }
      class NonDefinedMock {
        def defined: Boolean = false
      }
      (new DefinedMock) must be ('defined)
      (new NonDefinedMock) must not { be ('defined) }
      // (new NonDefinedMock) mustNot be ('defined)
    }

    it("must throw TestFailedException if no defined or isDefined method") {
      class DefinedMock {
        override def toString = "DefinedMock"
      }
      class NonDefinedMock {
        override def toString = "NonDefinedMock"
      }
      val ex1 = intercept[TestFailedException] {
        (new DefinedMock) must be ('defined)
      }
      ex1.getMessage must equal ("DefinedMock has neither a defined nor an isDefined method")
      val ex2 = intercept[TestFailedException] {
        (new NonDefinedMock) must not { be ('defined) }
      }
      ex2.getMessage must equal ("NonDefinedMock has neither a defined nor an isDefined method")
    }

    it("must call the Scala-style method if both a defined and an isDefined method exist") {
      class DefinedMock {
        def defined: Boolean = true
        def isDefined: Boolean = false
        override def toString = "DefinedMock"
      }
      class NonDefinedMock {
        def defined: Boolean = false
        def isDefined: Boolean = true
        override def toString = "NonDefinedMock"
      }
      (new DefinedMock) must be ('defined)
      (new NonDefinedMock) must not { be ('defined) }
    }

    it("must access an 'defined' val") {
      class DefinedMock {
        val defined: Boolean = true
      }
      class NonDefinedMock {
        val defined: Boolean = false
      }
      (new DefinedMock) must be ('defined)
      (new NonDefinedMock) must not { be ('defined) }
      // (new NonDefinedMock) mustNot be ('defined)
    }
  }
}
