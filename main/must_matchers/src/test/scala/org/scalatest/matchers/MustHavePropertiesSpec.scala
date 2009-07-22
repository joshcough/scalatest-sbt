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
import scala.reflect.BeanProperty

// TODO: check not not and not not not to make sure those negative failure messages make sense.
class MustHavePropertiesSpec extends Spec with MustMatchers with Checkers with ReturnsNormallyThrowsAssertion with BookPropertyMatchers {

  // Checking for a specific size
  describe("The 'have (' syntax") {

    describe("on an object with properties") {

      val book = new Book("A Tale of Two Cities", "Dickens", 1859, 45, true)
      val badBook = new Book("A Tale of Two Cities", "Dickens", 1859, 45, false)
      val bookshelf = new Bookshelf(book, badBook, book)

      it("must do nothing if there's just one property and it matches") {
        book must have (title ("A Tale of Two Cities"))
        book must have ('title ("A Tale of Two Cities"))
      }

      it("must do nothing if all the properties match") {
        book must have (
          title ("A Tale of Two Cities"),
          author ("Dickens"),
          pubYear (1859)
        )
        book must have (
          'title ("A Tale of Two Cities"),
          'author ("Dickens"),
          'pubYear (1859)
        )
      }

      it("must do nothing if there's just one property and it does not match, when used with not") {
        book must not have (title ("One Hundred Years of Solitude"))
        book must not have ('title ("One Hundred Years of Solitude"))
      }

      // title/author matches | have | have not
      // 0 0 | 0 | 1
      // 0 1 | 0 | 1
      // 1 0 | 0 | 1
      // 1 1 | 1 | 0
      it("must do nothing if at least one of the properties does not match, when used with not") {

        // 0 0 
        book must not have (
          title ("Moby Dick"),
          author ("Melville")
        )
        book must not have (
          'title ("Moby Dick"),
          'author ("Melville")
        )

        // 0 1 
        book must not have (
          title ("Moby Dick"),
          author ("Dickens")
        )
        book must not have (
          'title ("Moby Dick"),
          'author ("Dickens")
        )

        // 1 0 
        book must not have (
          title ("A Tale of Two Cities"),
          author ("Melville")
        )
        book must not have (
          'title ("A Tale of Two Cities"),
          'author ("Melville")
        )
      }

      it("must do nothing if all properties match, when used with and") {
        book must (have (title ("A Tale of Two Cities")) and (have (author ("Dickens"))))
        book must (have (title ("A Tale of Two Cities")) and have (author ("Dickens")))
        book must (have ('title ("A Tale of Two Cities")) and (have ('author ("Dickens"))))
        book must (have ('title ("A Tale of Two Cities")) and have ('author ("Dickens")))
      }

      it("must do nothing if at least one property matches, when used with or") {

        // both true
        book must (have (title ("A Tale of Two Cities")) or (have (author ("Dickens"))))
        book must (have (title ("A Tale of Two Cities")) or have (author ("Dickens")))
        book must (have ('title ("A Tale of Two Cities")) or (have ('author ("Dickens"))))
        book must (have ('title ("A Tale of Two Cities")) or have ('author ("Dickens")))

        // first true
        book must (have (title ("A Tale of Two Cities")) or (have (author ("Melville"))))
        book must (have (title ("A Tale of Two Cities")) or have (author ("Melville")))
        book must (have ('title ("A Tale of Two Cities")) or (have ('author ("Melville"))))
        book must (have ('title ("A Tale of Two Cities")) or have ('author ("Melville")))

        // second true
        book must (have (title ("Moby Dick")) or (have (author ("Dickens"))))
        book must (have (title ("Moby Dick")) or have (author ("Dickens")))
        book must (have ('title ("Moby Dick")) or (have ('author ("Dickens"))))
        book must (have ('title ("Moby Dick")) or have ('author ("Dickens")))
      }

      it("must do nothing if no properties match, when used with and and not") {

        // just one property
        book must (not have (title ("Moby Dick")) and (not have (author ("Melville"))))
        book must (not have (title ("Moby Dick")) and not (have (author ("Melville"))))
        book must (not have (title ("Moby Dick")) and not have (author ("Melville")))
        book must (not have ('title ("Moby Dick")) and (not have ('author ("Melville"))))
        book must (not have ('title ("Moby Dick")) and not (have ('author ("Melville"))))
        book must (not have ('title ("Moby Dick")) and not have ('author ("Melville")))

        // multiple properties
        book must (not have (title ("Moby Dick"), pubYear (1859)) and (not have (pubYear (1859), author ("Melville"))))
        book must (not have (title ("Moby Dick"), pubYear (1859)) and not (have (pubYear (1859), author ("Melville"))))
        book must (not have (title ("Moby Dick"), pubYear (1859)) and not have (pubYear (1859), author ("Melville")))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) and (not have ('pubYear (1859), 'author ("Melville"))))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) and not (have ('pubYear (1859), 'author ("Melville"))))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) and not have ('pubYear (1859), 'author ("Melville")))
      }

      it("must do nothing if no properties match, when used with or and not") {

        // both true
        // just one property
        book must (not have (title ("Moby Dick")) or (not have (author ("Melville"))))
        book must (not have (title ("Moby Dick")) or not (have (author ("Melville"))))
        book must (not have (title ("Moby Dick")) or not have (author ("Melville")))
        book must (not have ('title ("Moby Dick")) or (not have ('author ("Melville"))))
        book must (not have ('title ("Moby Dick")) or not (have ('author ("Melville"))))
        book must (not have ('title ("Moby Dick")) or not have ('author ("Melville")))

        // multiple properties
        book must (not have (title ("Moby Dick"), pubYear (1859)) or (not have (pubYear (1859), author ("Melville"))))
        book must (not have (title ("Moby Dick"), pubYear (1859)) or not (have (pubYear (1859), author ("Melville"))))
        book must (not have (title ("Moby Dick"), pubYear (1859)) or not have (pubYear (1859), author ("Melville")))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) or (not have ('pubYear (1859), 'author ("Melville"))))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) or not (have ('pubYear (1859), 'author ("Melville"))))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) or not have ('pubYear (1859), 'author ("Melville")))

        // first true
        // just one property
        book must (not have (title ("Moby Dick")) or (not have (author ("Dickens"))))
        book must (not have (title ("Moby Dick")) or not (have (author ("Dickens"))))
        book must (not have (title ("Moby Dick")) or not have (author ("Dickens")))
        book must (not have ('title ("Moby Dick")) or (not have ('author ("Dickens"))))
        book must (not have ('title ("Moby Dick")) or not (have ('author ("Dickens"))))
        book must (not have ('title ("Moby Dick")) or not have ('author ("Dickens")))

        // multiple properties
        book must (not have (title ("Moby Dick"), pubYear (1859)) or (not have (pubYear (1859), author ("Dickens"))))
        book must (not have (title ("Moby Dick"), pubYear (1859)) or not (have (pubYear (1859), author ("Dickens"))))
        book must (not have (title ("Moby Dick"), pubYear (1859)) or not have (pubYear (1859), author ("Dickens")))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) or (not have ('pubYear (1859), 'author ("Dickens"))))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) or not (have ('pubYear (1859), 'author ("Dickens"))))
        book must (not have ('title ("Moby Dick"), pubYear (1859)) or not have ('pubYear (1859), 'author ("Dickens")))

        // second true
        // just one property
        book must (not have (title ("A Tale of Two Cities")) or (not have (author ("Melville"))))
        book must (not have (title ("A Tale of Two Cities")) or not (have (author ("Melville"))))
        book must (not have (title ("A Tale of Two Cities")) or not have (author ("Melville")))
        book must (not have ('title ("A Tale of Two Cities")) or (not have ('author ("Melville"))))
        book must (not have ('title ("A Tale of Two Cities")) or not (have ('author ("Melville"))))
        book must (not have ('title ("A Tale of Two Cities")) or not have ('author ("Melville")))

        // multiple properties
        book must (not have (title ("A Tale of Two Cities"), pubYear (1859)) or (not have (pubYear (1859), author ("Melville"))))
        book must (not have (title ("A Tale of Two Cities"), pubYear (1859)) or not (have (pubYear (1859), author ("Melville"))))
        book must (not have (title ("A Tale of Two Cities"), pubYear (1859)) or not have (pubYear (1859), author ("Melville")))
        book must (not have ('title ("A Tale of Two Cities"), pubYear (1859)) or (not have ('pubYear (1859), 'author ("Melville"))))
        book must (not have ('title ("A Tale of Two Cities"), pubYear (1859)) or not (have ('pubYear (1859), 'author ("Melville"))))
        book must (not have ('title ("A Tale of Two Cities"), pubYear (1859)) or not have ('pubYear (1859), 'author ("Melville")))
      }

      it("must throw TestFailedException if there's just one property and it doesn't match") {

        val caught1 = intercept[TestFailedException] {
          book must have (author ("Gibson"))
        }
        assert(caught1.getMessage === "The author property had value \"Dickens\", instead of its expected value \"Gibson\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught2 = intercept[TestFailedException] {
          book must have ('author ("Gibson"))
        }
        assert(caught2.getMessage === "The author property had value \"Dickens\", instead of its expected value \"Gibson\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if at least one of the properties doesn't match") {

        val caught1 = intercept[TestFailedException] {
          book must have (
            title ("A Tale of Two Cities"),
            author ("Gibson"),
            pubYear (1859)
          )
        }
        assert(caught1.getMessage === "The author property had value \"Dickens\", instead of its expected value \"Gibson\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught2 = intercept[TestFailedException] {
          book must have (
            title ("A Tale of Two Cities"),
            'author ("Gibson"),
            pubYear (1859)
          )
        }
        assert(caught2.getMessage === "The author property had value \"Dickens\", instead of its expected value \"Gibson\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught3 = intercept[TestFailedException] {
          book must have (
            'title ("A Tale of Two Cities"),
            'author ("Dickens"),
            'pubYear (1959)
          )
        }
        assert(caught3.getMessage === "The pubYear property had value 1859, instead of its expected value 1959, on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if there's just one property and it matches, when used with not") {

        val caught1 = intercept[TestFailedException] {
          book must not have (author ("Dickens"))
        }
        assert(caught1.getMessage === "The author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught2 = intercept[TestFailedException] {
          book must not have ('author ("Dickens"))
        }
        assert(caught2.getMessage === "The author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      /*
      Not (matcher) needs to yield the opposite result as (matcher) itself, and
      that means that not (matcher) will be true if at least one 

      title/author/pubYear matches | have | not have
      0 0 0 | 0 | 1
      0 0 1 | 0 | 1
      0 1 0 | 0 | 1
      0 1 1 | 0 | 1
      1 0 0 | 0 | 1
      1 0 1 | 0 | 1
      1 1 0 | 0 | 1
      1 1 1 | 1 | 0

      So 'not have" means that at least one is false, not all are false.

      To reduce the number of tests cases just use two:

      title/author matches | have | have not
      0 0 | 0 | 1
      0 1 | 0 | 1
      1 0 | 0 | 1
      1 1 | 1 | 0


      have matches (1 1) all properties matched.
      have does not match (0 0, 0 1, 1 0) the (first property found that doesn't match) didn't match
      not have matches (0 0, 0 1, 1 0) the (first property found that doesn't match), as expected
      not have does not match (1, 1) all properties matched.
      */
      it("must throw TestFailedException if all of the properties match, when used with not") {
        val caught1 = intercept[TestFailedException] {
          book must not have (
            title ("A Tale of Two Cities"),
            author ("Dickens")
          )
        }
        assert(caught1.getMessage === "All properties had their expected values, respectively, on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if at least one property does not match, when used with and") {

        // second false
        val caught1 = intercept[TestFailedException] {
          book must (have (title ("A Tale of Two Cities")) and (have (author ("Melville"))))
        }
        assert(caught1.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught2 = intercept[TestFailedException] {
          book must (have (title ("A Tale of Two Cities")) and have (author ("Melville")))
        }
        assert(caught2.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught3 = intercept[TestFailedException] {
          book must (have ('title ("A Tale of Two Cities")) and (have ('author ("Melville"))))
        }
        assert(caught3.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught4 = intercept[TestFailedException] {
          book must (have ('title ("A Tale of Two Cities")) and have ('author ("Melville")))
        }
        assert(caught4.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        // first false
        val caught11 = intercept[TestFailedException] {
          book must (have (title ("Moby Dick")) and (have (author ("Dickens"))))
        }
        assert(caught11.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught12 = intercept[TestFailedException] {
          book must (have (title ("Moby Dick")) and have (author ("Dickens")))
        }
        assert(caught12.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught13 = intercept[TestFailedException] {
          book must (have ('title ("Moby Dick")) and (have ('author ("Dickens"))))
        }
        assert(caught13.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught14 = intercept[TestFailedException] {
          book must (have ('title ("Moby Dick")) and have ('author ("Dickens")))
        }
        assert(caught14.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        // both false
        val caught21 = intercept[TestFailedException] {
          book must (have (title ("Moby Dick")) and (have (author ("Melville"))))
        }
        assert(caught21.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught22 = intercept[TestFailedException] {
          book must (have (title ("Moby Dick")) and have (author ("Melville")))
        }
        assert(caught22.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught23 = intercept[TestFailedException] {
          book must (have ('title ("Moby Dick")) and (have ('author ("Melville"))))
        }
        assert(caught23.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught24 = intercept[TestFailedException] {
          book must (have ('title ("Moby Dick")) and have ('author ("Melville")))
        }
        assert(caught24.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if neither property matches, when used with or") {

        // both false
        val caught21 = intercept[TestFailedException] {
          book must (have (title ("Moby Dick")) or (have (author ("Melville"))))
        }
        assert(caught21.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught22 = intercept[TestFailedException] {
          book must (have (title ("Moby Dick")) or have (author ("Melville")))
        }
        assert(caught22.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught23 = intercept[TestFailedException] {
          book must (have ('title ("Moby Dick")) or (have ('author ("Melville"))))
        }
        assert(caught23.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught24 = intercept[TestFailedException] {
          book must (have ('title ("Moby Dick")) or have ('author ("Melville")))
        }
        assert(caught24.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had value \"Dickens\", instead of its expected value \"Melville\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if at least one property does not match, when used with and and not") {

        // second false
        val caught1 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) and not (have (author ("Melville"))))
        }
        assert(caught1.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught2 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) and not have (author ("Melville")))
        }
        assert(caught2.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught3 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) and not (have ('author ("Melville"))))
        }
        assert(caught3.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught4 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) and not have ('author ("Melville")))
        }
        assert(caught4.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught5 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) and (not have (author ("Melville"))))
        }
        assert(caught5.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught6 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) and (not have ('author ("Melville"))))
        }
        assert(caught6.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        // first false
        val caught11 = intercept[TestFailedException] {
          book must (not have (title ("Moby Dick")) and not (have (author ("Dickens"))))
        }
        assert(caught11.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught12 = intercept[TestFailedException] {
          book must (not have (title ("Moby Dick")) and not have (author ("Dickens")))
        }
        assert(caught12.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught13 = intercept[TestFailedException] {
          book must (not have ('title ("Moby Dick")) and (not have ('author ("Dickens"))))
        }
        assert(caught13.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught14 = intercept[TestFailedException] {
          book must (not have ('title ("Moby Dick")) and not have ('author ("Dickens")))
        }
        assert(caught14.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught15 = intercept[TestFailedException] {
          book must (not have (title ("Moby Dick")) and (not have (author ("Dickens"))))
        }
        assert(caught15.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught16 = intercept[TestFailedException] {
          book must (not have ('title ("Moby Dick")) and (not have ('author ("Dickens"))))
        }
        assert(caught16.getMessage === "The title property had value \"A Tale of Two Cities\", instead of its expected value \"Moby Dick\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), but the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        // both true
        val caught21 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) and (not have (author ("Dickens"))))
        }
        assert(caught21.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught22 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) and not have (author ("Dickens")))
        }
        assert(caught22.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught23 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) and (not have ('author ("Dickens"))))
        }
        assert(caught23.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught24 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) and not have ('author ("Dickens")))
        }
        assert(caught24.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught25 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) and (not (have (author ("Dickens")))))
        }
        assert(caught25.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught26 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) and (not (have ('author ("Dickens")))))
        }
        assert(caught26.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if both properties match, when used with or and not") {

        // both false
        val caught21 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) or (not have (author ("Dickens"))))
        }
        assert(caught21.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught22 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) or not have (author ("Dickens")))
        }
        assert(caught22.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught23 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) or (not have ('author ("Dickens"))))
        }
        assert(caught23.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught24 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) or not have ('author ("Dickens")))
        }
        assert(caught24.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught25 = intercept[TestFailedException] {
          book must (not have (title ("A Tale of Two Cities")) or (not have (author ("Dickens"))))
        }
        assert(caught25.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        val caught26 = intercept[TestFailedException] {
          book must (not have ('title ("A Tale of Two Cities")) or (not have ('author ("Dickens"))))
        }
        assert(caught26.getMessage === "The title property had its expected value \"A Tale of Two Cities\", on object Book(A Tale of Two Cities,Dickens,1859,45,true), and the author property had its expected value \"Dickens\", on object Book(A Tale of Two Cities,Dickens,1859,45,true)")

        // A double one, so that I can see the mid-sentence version of the 'all properties...' error message
        val caught31 = intercept[TestFailedException] {
          book must (
            not have (
              title ("A Tale of Two Cities"),
              author ("Dickens")
            ) or not have (
              author ("Dickens"),
              pubYear (1859)
            )
          )
        }
        assert(caught31.getMessage === "All properties had their expected values, respectively, on object Book(A Tale of Two Cities,Dickens,1859,45,true), and all properties had their expected values, respectively, on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if a nested property matcher expression is used and a nested property doesn't match") {

        // I'm not too hot on this syntax, but can't prevent it and wouldn't want to. If people want do to nested property
        // checks, they can do it this way.
        val caught1 = intercept[TestFailedException] {
          bookshelf must have (
            book1 (
              title ("A Tale of Two Cities"),
              author ("Gibson"),
              pubYear (1859)
            )
          )
        }
        assert(caught1.getMessage === "The book1.author property had value \"Dickens\", instead of its expected value \"Gibson\", on object Bookshelf(Book(A Tale of Two Cities,Dickens,1859,45,true),Book(A Tale of Two Cities,Dickens,1859,45,false),Book(A Tale of Two Cities,Dickens,1859,45,true))")
      }

      it("must work with length not a symbol without anything special, in case someone forgets you don't need the parens with length") {

        val caught1 = intercept[TestFailedException] {
          book must have (length (43))
        }
        assert(caught1.getMessage === "The length property had value 45, instead of its expected value 43, on object Book(A Tale of Two Cities,Dickens,1859,45,true)")
      }

      it("must throw TestFailedException if length used in parens but the length property is not an integral type") {

        class LengthSeven {
          def length = "seven"
        }

        val caught1 = intercept[TestFailedException] {
          (new LengthSeven) must have (length (43))
        }
        assert(caught1.getMessage === "The length property was none of Byte, Short, Int, or Long.")
      }

      it("must work with size not a symbol without anything special, in case someone forgets you don't need the parens with size") {

        case class Size(val size: Int)

        val caught1 = intercept[TestFailedException] {
          (new Size(7)) must have (size (43))
        }
        assert(caught1.getMessage === "The size property had value 7, instead of its expected value 43, on object Size(7)")
      }

      it("must throw TestFailedException if size used in parens but the size property is not an integral type") {

        class SizeSeven {
          def size = "seven"
        }

        val caught1 = intercept[TestFailedException] {
          (new SizeSeven) must have (size (43))
        }
        assert(caught1.getMessage === "The size property was none of Byte, Short, Int, or Long.")
      }

/*
I decided not to support this syntax in 0.9.5, and maybe never. It is not clear to me that it is
readable enough. I can't prevent someone from making HavePropertyMatchers to do this kind of thing,
and that's fine. It actually gives them a way to do it if they want to do it.
      it("must throw TestFailedException if a nested property matcher expression with a symbol is used and a nested property doesn't match") {

        val caught1 = intercept[TestFailedException] {
          bookshelf must have (
            'book1 (
              title ("A Tale of Two Cities"),
              author ("Gibson"),
              pubYear (1859)
            )
          )
        }
        assert(caught1.getMessage === "expected property book1.author to have value \"Gibson\", but it had value \"Dickens\"")
      }
*/

      /*
      This does not compile, which is what I want
      it("must not compile if you don't enter any verifiers") {
        book must have ()
      }
      */
    }
  }
}