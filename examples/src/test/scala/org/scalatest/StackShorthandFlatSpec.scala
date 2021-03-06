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
package org.scalatest

import matchers.ShouldVerb

class StackShorthandFlatSpec extends FlatSpec with StackFixtureCreationMethods with FlatStackBehaviors with ShouldVerb {

  "A Stack (when empty)" should "be empty" in {
    assert(emptyStack.empty)
  }

  it should "complain on peek" in {
    intercept[IllegalStateException] {
      emptyStack.peek
    }
  }

  ignore should "complain on pop" in {
    intercept[IllegalStateException] {
      emptyStack.pop
    }
  }

  "A Stack (with one item)" should
          behave like nonEmptyStack(lastValuePushed)(stackWithOneItem)

  it should behave like nonFullStack(stackWithOneItem)

  "A Stack (with one item less than capacity)" should
          behave like nonEmptyStack(lastValuePushed)(stackWithOneItemLessThanCapacity)

  it should behave like nonFullStack(stackWithOneItemLessThanCapacity)

  "A Stack (full)" should "be full" in {
    assert(fullStack.full)
  }

  it should "go to sleep soon" in (pending)

  it should behave like nonEmptyStack(lastValuePushed)(fullStack)

  it should "complain on a push" in {
    intercept[IllegalStateException] {
      fullStack.push(10)
    }
  }
}
