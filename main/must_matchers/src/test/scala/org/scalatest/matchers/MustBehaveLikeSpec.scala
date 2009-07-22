/*
 * Copyright 2001-2009 Artima, Inc.
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

class MustBehaveLikeSpec extends Spec {

  def myFirstBehavior(i: Int) {
    it("This one is must blow up") {}
  }

  describe("The 'must behave like' syntax must throw an exception inside an it clause") {
    it("the code in here must fail with an exception") {
      intercept[TestRegistrationClosedException] {
        it must behave like myFirstBehavior(1) 
      }
    }
  }

  // Checking for a specific size
/*
  describe("The 'must behave like' syntax must work in a describe") {

    it must behave like nonEmptyStack(lastValuePushed)(stackWithOneItem) 

    describe(", and in a nested describe") {

      it must behave like nonEmptyStack(lastValuePushed)(stackWithOneItem) 
    }
  }
*/

  def myBehavior(i: Int) {
    it("This one is solo") {}
  }
  it must behave like myBehavior(1) 

  // TODO: Make these into real tests. I looked at it and heck they work. So I can indeed put describe clauses in
  // the shared behaviors. Cool.
  def myNestedBehavior(i: Int) {
    describe("and this is the shared describe") {
      it("This one is nested") {}
    }
  }

  it must behave like myNestedBehavior(1) 
  describe("And outer describe...") {
    it must behave like myNestedBehavior(1) 
  }

/* Correct, none of these compiled
  describe("'must not behave like' must not compile") {
    
    stackWithOneItem must not behave like (nonEmptyStack(lastValuePushed))
  }
  describe("The 'must behave like' syntax in an and or or clause, with or without not, must not compile") {
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) or behave like (nonFullStack))
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) or (behave like (nonFullStack)))
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) or not behave like (nonFullStack))
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) or (not behave like (nonFullStack)))
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) and behave like (nonFullStack))
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) and (behave like (nonFullStack)))
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) and not behave like (nonFullStack))
    stackWithOneItem must (behave like (nonEmptyStack(lastValuePushed)) and (not behave like (nonFullStack)))
  }
*/
}
