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

import scala.reflect.Manifest
import Helper.newTestFailedException

/**
 * Trait that provides a domain specific language (DSL) for expressing assertions in tests
 * using the word <code>must</code>. (If you prefer the word <code>should</code>, you can alternatively
 * mix in trait <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a>.) For example, if you mix <code>MustMatchers</code> into
 * a suite class, you can write an equality assertion in that suite like this:
 * 
 * <pre class="indent">
 * object must equal (3)
 * </pre>
 * 
 * <p>
 * Here <code>object</code> is a variable, and can be of any type. If the object is an
 * <code>Int</code> with the value 3, execution will continue (<em>i.e.</em>, the expression will result
 * in the unit value, <code>()</code>). Otherwise, a <code>TestFailedException</code>
 * will be thrown with a detail message that explains the problem, such as <code>"7 did not equal 3"</code>.
 * This <code>TestFailedException</code> will cause the test to fail.
 * </p>
 * 
 * <p>
 * The <code>left must equal (right)</code> syntax works by calling <code>==</code>  on the <code>left</code>
 * value, passing in the <code>right</code> value, on every type except arrays. If <code>left</code> is an array, <code>deepEquals</code>
 * will be invoked on <code>left</code>, passing in <code>right</code>. Thus, even though this expression
 * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
 * </p>
 * 
 * <pre class="indent">
 * Array(1, 2) == Array(1, 2) // yields false
 * </pre>
 *
 * <p>
 * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because <code>deepEquals</code> compares
 * the two arrays structurally, taking into consideration the equality of the array's contents:
 * </p>
 *
 * <pre class="indent">
 * Array(1, 2) must equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
 * <code>be theSameInstanceAs</code> syntax, described below.
 * </p>
 *
 * <h2>Checking size and length</h2>
 * 
 * <p>
 * You can check the size or length of just about any type of object for which it
 * would make sense. Here's how checking for length looks:
 * </p>
 * <pre class="indent">
 * object must have length (3)
 * </pre>
 * 
 * <p>
 *Size is similar:
 * </p>
 * 
 * <pre class="indent">
 * object must have size (10)
 * </pre>
 * 
 * <p>
 * The <code>length</code> syntax can be used with any object that has a field or method named <code>length</code>
 * or a method named <code>getLength</code>.   Similarly, the <code>size</code> syntax can be used with any
 * object that has a field or method named <code>size</code> or a method named <code>getSize</code>.
 * The type of a <code>length</code> or <code>size</code> field, or return type of a method, must be either <code>Int</code>
 * or <code>Long</code>. Any such method must take no parameters. (The Scala compiler will ensure at compile time that
 * the object on which <code>must</code> is being invoked has the appropriate structure.)
 * </p>
 * 
 * <h2>Checking strings</h2>
 * 
 * <p>
 * You can check for whether a string starts with, ends with, or includes a substring like this:
 * </p>
 * 
 * <pre class="indent">
 * string must startWith ("Hello")
 * string must endWith ("world")
 * string must include ("seven")
 * </pre>
 * 
 * <p>
 * You can check for whether a string starts with, ends with, or includes a regular expression, like this:
 * </p>
 * 
 * <pre class="indent">
 * string must startWith regex ("Hel*o")
 * string must endWith regex ("wo.ld")
 * string must include regex ("wo.ld")
 * </pre>
 * 
 * <p>
 * And you can check whether a string fully matches a regular expression, like this:
 * </p>
 * 
 * <pre class="indent">
 * string must fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
 * </pre>
 * 
 * <p>
 * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
 * or a <code>scala.util.matching.Regex</code>.
 * </p>
 *
 * <h2>Greater and less than</h2>
 * <p>
 * You can check whether any type that is, or can be implicitly converted to,
 * an <code>Ordered[T]</code> is greater than, less than, greater than or equal, or less
 * than or equal to a value of type <code>T</code>. The syntax is:
 * </p>
 * <pre class="indent">
 * one must be < (7)
 * one must be > (0)
 * one must be <= (7)
 * one must be >= (0)
 * </pre>
 * 
 * <h2>Checking <code>Boolean</code> properties with <code>be</code></h2>
 * 
 * <p>
 * If an object has a method that takes no parameters and returns boolean, you can check
 * it by placing a <code>Symbol</code> (after <code>be</code>) that specifies the name
 * of the method (excluding an optional prefix of "<code>is</code>"). A symbol literal
 * in Scala begins with a tick mark and ends at the first non-identifier character. Thus,
 * <code>'empty</code> results in a <code>Symbol</code> object at runtime, as does
 * <code>'defined</code> and <code>'file</code>. Here's an example:
 * </p>
 * 
 * <pre class="indent">
 * emptySet must be ('empty)
 * </pre>
 * 
 * Given this code, ScalaTest will use reflection to look on the object referenced from
 * <code>emptySet</code> for a method that takes no parameters and results in <code>Boolean</code>,
 * with either the name <code>empty</code> or <code>isEmpty</code>. If found, it will invoke
 * that method. If the method returns <code>true</code>, execution will continue. But if it returns
 * <code>false</code>, a <code>TestFailedException</code> will be thrown that will contain a detail message, such as:
 * 
 * <pre class="indent">
 * Set(1, 2, 3) was not empty
 * </pre>
 * 
 * <p>
 * This <code>be</code> syntax can be used with any type.  If the object does
 * not have an appropriately named predicate method, you'll get a <code>TestFailedException</code>
 * at runtime with a detail message that explains the problem.
 * (For the details on how a field or method is selected during this
 * process, see the documentation for <a href="Matchers.BeWord.html"><code>BeWord</code></a>.)
 * </p>
 * 
 * <p>
 * If you think it reads better, you can optionally put <code>a</code> or <code>an</code> after
 * <code>be</code>. For example, <code>java.io.File</code> has two predicate methods,
 * <code>isFile</code> and <code>isDirectory</code>. Thus with a <code>File</code> object
 * named <code>temp</code>, you could write:
 * </p>
 * 
 * <pre class="indent">
 * temp must be a ('file)
 * </pre>
 * 
 * <p>
 * Or, given <code>java.awt.event.KeyEvent</code> has a method <code>isActionKey</code> that takes
 * no arguments and returns <code>Boolean</code>, you could assert that a <code>KeyEvent</code> is
 * an action key with:
 *</p>
 *
 * <pre class="indent">
 * keyEvent must be an ('actionKey)
 * </pre>
 * 
 * <p>
 * If you prefer to check <code>Boolean</code> properties in a type-safe manner, you can use a <code>BePropertyMatcher</code>.
 * This would allow you to write expressions such as:
 * </p>
 *
 * <pre class="indent">
 * emptySet must be (empty)
 * temp must be a (file)
 * keyEvent must be an (actionKey)
 * </pre>
 * 
 * <p>
 * These expressions would fail to compile if <code>must</code> is used on an inappropriate type, as determined
 * by the type parameter of the <code>BePropertyMatcher</code> being used. (For example, <code>file</code> in this example
 * would likely be of type <code>BePropertyMatcher[java.io.File]</code>. If used with an appropriate type, such an expression will compile
 * and at run time the <code>Boolean</code> property method or field will be accessed directly; <em>i.e.</em>, no reflection will be used.
 * See the documentation for <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a> for more information.
 * </p>
 *
 * <h2>Using custom <code>BeMatchers</code></h2>
 *
 * If you want to create a new way of using <code>be</code>, which doesn't map to an actual property on the
 * type you care about, you can create a <code>BeMatcher</code>. You could use this, for example, to create <code>BeMatcher[Int]</code>
 * called <code>odd</code>, which would match any odd <code>Int</code>, and <code>even</code>, which would match
 * any even <code>Int</code>. 
 * Given this pair of <code>BeMatcher</code>s, you could check whether an <code>Int</code> was odd or even with expressions like:
 * </p>
 *
 * <pre class="indent">
 * num must be (odd)
 * num must not be (even)
 * </pre>
 *
 * For more information, see the documentation for <a href="BeMatcher.html"><code>BeMatcher</code></a>.
 *
 * <h2>Checking object identity</h2>
 * 
 * <p>
 * If you need to check that two references refer to the exact same object, you can write:
 * </p>
 * 
 * <pre class="indent">
 * ref1 must be theSameInstanceAs (ref2)
 * </pre>
 * 
 * <h2>Checking numbers against a range</h2>
 * 
 * <p>
 * To check whether a floating point number has a value that exactly matches another, you
 * can use <code>must equal</code>:
 * </p>
 * 
 * <pre class="indent">
 * sevenDotOh must equal (7.0)
 * </pre>
 * 
 * <p>
 * Often, however, you may want to check whether a floating point number is within a
 * range. You can do that using <code>be</code> and <code>plusOrMinus</code>, like this:
 * </p>
 * 
 * <pre class="indent">
 * sevenDotOh must be (6.9 plusOrMinus 0.2)
 * </pre>
 * 
 * <p>
 * This expression will cause a <code>TestFailedException</code> to be thrown if the floating point
 * value, <code>sevenDotOh</code> is outside the range <code>6.7</code> to <code>7.1</code>.
 * You can also use <code>plusOrMinus</code> with integral types, for example:
 * </p>
 * 
 * <pre class="indent">
 * seven must be (6 plusOrMinus 2)
 * </pre>
 * 
 * <h2>Iterables, collections, sequences, and maps</h2>
 * 
 * <p>
 * You can use some of the syntax shown previously with <code>Iterable</code> and its
 * subtypes. For example, you can check whether an <code>Iterable</code> is <code>empty</code>,
 * like this:
 * </p>
 * 
 * <pre class="indent">
 * iterable must be ('empty)
 * </pre>
 * 
 * <p>
 * You can check the length of an <code>Seq</code> (<code>Array</code>, <code>List</code>, etc.),
 * like this:
 * </p>
 * 
 * <pre class="indent">
 * array must have length (3)
 * list must have length (9)
 * </pre>
 * 
 * <p>
 * You can check the size of any <code>Collection</code>, like this:
 * </p>
 * 
 * <pre class="indent">
 * map must have size (20)
 * set must have size (90)
 * </pre>
 * 
 * <p>
 * In addition, you can check whether an <code>Iterable</code> contains a particular
 * element, like this:
 * </p>
 * 
 * <pre class="indent">
 * iterable must contain ("five")
 * </pre>
 * 
 * <p>
 * You can also check whether a <code>Map</code> contains a particular key, or value, like this:
 * </p>
 * 
 * <pre class="indent">
 * map must contain key (1)
 * map must contain value ("Howdy")
 * </pre>
 * 
 * <h2>Java collections and maps</h2>
 * 
 * <p>
 * You can use similar syntax on Java collections (<code>java.util.Collection</code>) and maps (<code>java.util.Map</code>).
 * For example, you can check whether a Java <code>Collection</code> or <code>Map</code> is <code>empty</code>,
 * like this:
 * </p>
 * 
 * <pre class="indent">
 * javaCollection must be ('empty)
 * javaMap must be ('empty)
 * </pre>
 * 
 * <p>
 * Even though Java's <code>List</code> type doesn't actually have a <code>length</code> or <code>getLength</code> method,
 * you can nevertheless check the length of a Java <code>List</code> (<code>java.util.List</code>) like this:
 * </p>
 * 
 * <pre class="indent">
 * javaList must have length (9)
 * </pre>
 * 
 * <p>
 * You can check the size of any Java <code>Collection</code> or <code>Map</code>, like this:
 * </p>
 * 
 * <pre class="indent">
 * javaMap must have size (20)
 * javaSet must have size (90)
 * </pre>
 * 
 * <p>
 * In addition, you can check whether a Java <code>Collection</code> contains a particular
 * element, like this:
 * </p>
 * 
 * <pre class="indent">
 * javaCollection must contain ("five")
 * </pre>
 * 
 * <p>
 * One difference to note between the syntax supported on Java collections and that of Scala
 * iterables is that you can't use <code>contain (...)</code> syntax with a Java <code>Map</code>.
 * Java differs from Scala in that its <code>Map</code> is not a subtype of its <code>Collection</code> type.
 * If you want to check that a Java <code>Map</code> contains a specific key/value pair, the best approach is
 * to invoke <code>entrySet</code> on the Java <code>Map</code> and check that entry set for the appropriate
 * element (a <code>java.util.Map.Entry</code>) using <code>contain (...)</code>.
 * </p>
 *
 * <p>
 * Despite this difference, the other (more commonly used) map matcher syntax works just fine on Java <code>Map</code>s.
 * You can, for example, check whether a Java <code>Map</code> contains a particular key, or value, like this:
 * </p>
 * 
 * <pre class="indent">
 * javaMap must contain key (1)
 * javaMap must contain value ("Howdy")
 * </pre>
 * 
 * <h2>Be as an equality comparison</h2>
 * 
 * <p>
 * All uses of <code>be</code> other than those shown previously perform an equality comparison. In other words, they work
 * the same as <code>equals</code>. This redundance between <code>be</code> and <code>equals</code> exists because it enables syntax
 * that sometimes sounds more natural. For example, instead of writing: 
 * </p>
 * 
 * <pre class="indent">
 * result must equal (null)
 * </pre>
 * 
 * <p>
 * You can write:
 * </p>
 * 
 * <pre class="indent">
 * result must be (null)
 * </pre>
 * 
 * <p>
 * (Hopefully you won't write that too much given <code>null</code> is error prone, and <code>Option</code>
 * is usually a better, well, option.) 
 * Here are some other examples of <code>be</code> used for equality comparison:
 * </p>
 * 
 * <pre class="indent">
 * sum must be (7.0)
 * boring must be (false)
 * fun must be (true)
 * list must be (Nil)
 * option must be (None)
 * option must be (Some(1))
 * </pre>
 * 
 * <p>
 * As with <code>equal</code>, using <code>be</code> on arrays results in <code>deepEquals</code> being called, not <code>equals</code>. As a result,
 * the following expression would <em>not</em> throw a <code>TestFailedException</code>:
 * </p>
 *
 * <pre class="indent">
 * Array(1, 2) must be (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * Because <code>be</code> is used in several ways in ScalaTest matcher syntax, just as it is used in many ways in English, one
 * potential point of confusion in the event of a failure is determining whether <code>be</code> was being used as an equality comparison or
 * in some other way, such as a property assertion. To make it more obvious when <code>be</code> is being used for equality, the failure
 * messages generated for those equality checks will include the word <code>equal</code> in them. For example, if this expression fails with a
 * <code>TestFailedException</code>:
 * </p>
 *
 * <pre class="indent">
 * option must be (Some(1))
 * </pre>
 *
 * <p>
 * The detail message in that <code>TestFailedException</code> will include the words <code>"equal to"</code> to signify <code>be</code>
 * was in this case being used for equality comparison:
 * </p>
 *
 * <pre class="indent">
 * Some(2) was not equal to Some(1)
 * </pre>
 *
 * <h2>Being negative</h2>
 * 
 * <p>
 * If you wish to check the opposite of some condition, you can simply insert <code>not</code> in the expression.
 * Here are a few examples:
 * </p>
 * 
 * <pre class="indent">
 * object must not be (null)
 * sum must not be <= (10)
 * mylist must not equal (yourList)
 * string must not startWith ("Hello")
 * </pre>
 * 
 * <h2>Logical expressions with <code>and</code> and <code>or</code></h2>
 * 
 * <p>
 * You can also combine matcher expressions with <code>and</code> and/or <code>or</code>, however,
 * you must place parentheses or curly braces around the <code>and</code> or <code>or</code> expression. For example, 
 * this <code>and</code>-expression would not compile, because the parentheses are missing:
 * </p>
 * 
 * <pre class="indent">
 * map must contain key ("two") and not contain value (7) // ERROR, parentheses missing!
 * </pre>
 * 
 * <p>
 * Instead, you need to write:
 * </p>
 * 
 * <pre class="indent">
 * map must (contain key ("two") and not contain value (7))
 * </pre>
 * 
 * <p>
 * Here are some more examples:
 * </p>
 * 
 * <pre class="indent">
 * number must (be > (0) and be <= (10))
 * option must (equal (Some(List(1, 2, 3))) or be (None))
 * string must (
 *   equal ("fee") or
 *   equal ("fie") or
 *   equal ("foe") or
 *   equal ("fum")
 * )
 * </pre>
 * 
 * <p>
 * Two differences exist between expressions composed of these <code>and</code> and <code>or</code> operators and the expressions you can write
 * on regular <code>Boolean</code>s using its <code>&&</code> and <code>||</code> operators. First, expressions with <code>and</code>
 * and <code>or</code> do not short-circuit. The following contrived expression, for example, would print <code>"hello, world!"</code>:
 * </p>
 *
 * <pre class="indent">
 * "yellow" must (equal ("blue") and equal { println("hello, world!"); "green" })
 * </pre>
 * 
 * <p>
 * In other words, the entire <code>and</code> or <code>or</code> expression is always evaluated, so you'll see any side effects
 * of the right-hand side even if evaluating
 * only the left-hand side is enough to determine the ultimate result of the larger expression. Failure messages produced by these
 * expressions will "short-circuit," however,
 * mentioning only the left-hand side if that's enough to determine the result of the entire expression. This "short-circuiting" behavior
 * of failure messages is intended
 * to make it easier and quicker for you to ascertain which part of the expression caused the failure. The failure message for the previous
 * expression, for example, would be:
 * </p>
 * 
 * <pre class="indent">
 * "yellow" did not equal "blue"
 * </pre>
 * 
 * <p>
 * Most likely this lack of short-circuiting would rarely be noticeable, because evaluating the right hand side will usually not
 * involve a side effect. One situation where it might show up, however, is if you attempt to <code>and</code> a <code>null</code> check on a variable with an expression
 * that uses the variable, like this:
 * </p>
 *
 * <pre class="indent">
 * map must (not be (null) and contain key ("ouch"))
 * </pre>
 * 
 * <p>
 * If <code>map</code> is <code>null</code>, the test will indeed fail, but with a <code>NullPointerException</code>, not a
 * <code>TestFailedException</code>. Here, the <code>NullPointerException</code> is the visible right-hand side effect. To get a
 * <code>TestFailedException</code>, you would need to check each assertion separately:
 * </p>
 *
 * <pre class="indent">
 * map must not be (null)
 * map must contain key ("ouch")
 * </pre>
 * 
 * <p>
 * If <code>map</code> is <code>null</code> in this case, the <code>null</code> check in the first expression will fail with
 * a <code>TestFailedException</code>, and the second expression will never be executed.
 * </p>
 *
 * <p>
 * The other difference with <code>Boolean</code> operators is that although <code>&&</code> has a higher precedence than <code>||</code>,
 * <code>and</code> and <code>or</code>
 * have the same precedence. Thus although the <code>Boolean</code> expression <code>(a || b && c)</code> will evaluate the <code>&&</code> expression
 * before the <code>||</code> expression, like <code>(a || (b && c))</code>, the following expression:
 * </p>
 * 
 * <pre class="indent">
 * collection must (contain (7) or contain (8) and have size (9))
 * </pre>
 * 
 * <p>
 * Will evaluate left to right, as:
 * </p>
 * 
 * <pre class="indent">
 * collection must ((contain (7) or contain (8)) and have size (9))
 * </pre>
 * 
 * <p>
 * If you really want the <code>and</code> part to be evaluated first, you'll need to put in parentheses, like this:
 * </p>
 * 
 * <pre class="indent">
 * collection must (contain (7) or (contain (8) and have size (9)))
 * </pre>
 * 
 * <h2>Working with <code>Option</code>s</h2>
 * 
 * <p>
 * ScalaTest matchers has no special support for <code>Option</code>s, but you can 
 * work with them quite easily using syntax shown previously. For example, if you wish to check
 * whether an option is <code>None</code>, you can write any of:
 * </p>
 * 
 * <pre class="indent">
 * option must equal (None)
 * option must be (None)
 * option must not be ('defined)
 * option must be ('empty)
 * </pre>
 * 
 * <p>
 * If you wish to check an option is defined, and holds a specific value, you can write either of:
 * </p>
 * 
 * <pre class="indent">
 * option must equal (Some("hi"))
 * option must be (Some("hi"))
 * </pre>
 * 
 * <p>
 * If you only wish to check that an option is defined, but don't care what it's value is, you can write:
 * </p>
 * 
 * <pre class="indent">
 * option must be ('defined)
 * </pre>
 * 
 * <h2>Checking arbitrary properties with <code>have</code></h2>
 * 
 * <p>
 * Using <code>have</code>, you can check properties of any type, where a <em>property</em> is an attribute of any
 * object that can be retrieved either by a public field, method, or JavaBean-style <code>get</code>
 * or <code>is</code> method, like this:
 * </p>
 * 
 * <pre class="indent">
 * book must have (
 *   'title ("Programming in Scala"),
 *   'author (List("Odersky", "Spoon", "Venners")),
 *   'pubYear (2008)
 * )
 * </pre>
 * 
 * <p>
 * This expression will use reflection to ensure the <code>title</code>, <code>author</code>, and <code>pubYear</code> properties of object <code>book</code>
 * are equal to the specified values. For example, it will ensure that <code>book</code> has either a public Java field or method
 * named <code>title</code>, or a public method named <code>getTitle</code>, that when invoked (or accessed in the field case) results
 * in a the string <code>"Programming in Scala"</code>. If all specified properties exist and have their expected values, respectively,
 * execution will continue. If one or more of the properties either does not exist, or exists but results in an unexpected value,
 * a <code>TestFailedException</code> will be thrown that explains the problem. (For the details on how a field or method is selected during this
 * process, see the documentation for <a href="Matchers.HavePropertyMatcherGenerator.html"><code>HavePropertyMatcherGenerator</code></a>.)
 * </p>
 * 
 * <p>
 * When you use this syntax, you must place one or more property values in parentheses after <code>have</code>, seperated by commas, where a <em>property
 * value</em> is a symbol indicating the name of the property followed by the expected value in parentheses. The only exceptions to this rule is the syntax
 * for checking size and length shown previously, which does not require parentheses. If you forget and put parentheses in, however, everything will
 * still work as you'd expect. Thus instead of writing:
 * </p>
 *
 * <pre class="indent">
 * array must have length (3)
 * set must have size (90)
 * </pre>
 * 
 * <p>
 * You can alternatively, write:
 * </p>
 *
 * <pre class="indent">
 * array must have (length (3))
 * set must have (size (90))
 * </pre>
 * 
 * <p>
 * If a property has a value different from the specified expected value, a <code>TestFailedError</code> will be thrown
 * with a detail message that explains the problem. For example, if you assert the following on
 * a <code>book</code> whose title is <code>Moby Dick</code>:
 * </p>
 *
 * <pre class="indent">
 * book must have ('title ("A Tale of Two Cities"))
 * </pre>
 *
 * <p>
 * You'll get a <code>TestFailedException</code> with this detail message:
 * </p>
 *
 * <pre class="indent">
 * The title property had value "Moby Dick", instead of its expected value "A Tale of Two Cities",
 * on object Book("Moby Dick", "Melville", 1851)
 * </pre>
 * 
 * <p>
 * If you prefer to check properties in a type-safe manner, you can use a <code>HavePropertyMatcher</code>.
 * This would allow you to write expressions such as:
 * </p>
 *
 * <pre class="indent">
 * book must have (
 *   title ("Programming in Scala"),
 *   author (List("Odersky", "Spoon", "Venners")),
 *   pubYear (2008)
 * )
 * </pre>
 * 
 * <p>
 * These expressions would fail to compile if <code>must</code> is used on an inappropriate type, as determined
 * by the type parameter of the <code>HavePropertyMatcher</code> being used. (For example, <code>title</code> in this example
 * might be of type <code>HavePropertyMatcher[org.publiclibrary.Book]</code>. If used with an appropriate type, such an expression will compile
 * and at run time the property method or field will be accessed directly; <em>i.e.</em>, no reflection will be used.
 * See the documentation for <a href="HavePropertyMatcher.html"><code>HavePropertyMatcher</code></a> for more information.
 * </p>
 *
 * <h2>Using custom matchers</h2>
 * 
 * <p>
 * If none of the built-in matcher syntax (or options shown so far for extending the syntax) satisfy a particular need you have, you can create
 * custom <code>Matcher</code>s that allow
 * you to place your own syntax directly after <code>must</code>. For example, class <code>java.io.File</code> has a method <code>exists</code>, which
 * indicates whether a file of a certain path and name exists. Because the <code>exists</code> method takes no parameters and returns <code>Boolean</code>,
 * you can call it using <code>be</code> with a symbol or <code>BePropertyMatcher</code>, yielding assertions like:
 * </p>
 * 
 * <pre class="indent">
 * file must be ('exists)  // using a symbol
 * file must be (inExistance)   // using a BePropertyMatcher
 * </pre>
 * 
 * <p>
 * Although these expressions will achieve your goal of throwing a <code>TestFailedException</code> if the file does not exist, they don't produce
 * the most readable code because the English is either incorrect or awkward. In this case, you might want to create a
 * custom <code>Matcher[java.io.File]</code>
 * named <code>exist</code>, which you could then use to write expressions like:
 * </p>
 *
 * <pre class="indent">
 * // using a plain-old Matcher
 * file must exist
 * file must not (exist)
 * file must (exist and have ('name ("temp.txt")))
 * </pre>
 * 
 * <p>
 * Note that when you use custom <code>Matcher</code>s, you will need to put parentheses around the custom matcher in more cases than with
 * the built-in syntax. For example you will often need the parentheses after <code>not</code>, as shown above. (There's no penalty for
 * always surrounding custom matchers with parentheses, and if you ever leave them off when they are needed, you'll get a compiler error.)
 * For more information about how to create custom <code>Matcher</code>s, please see the documentation for the <a href="Matcher.html"><code>Matcher</code></a> trait.
 * </p>
 *
 * <h2>Those pesky parens</h2>
 * 
 * <p>
 * Perhaps the most tricky part of writing assertions using ScalaTest matchers is remembering
 * when you need or don't need parentheses, but bearing in mind a few simple rules  should help.
 * It is also reassuring to know that if you ever leave off a set of parentheses when they are
 * required, your code will not compile. Thus the compiler will help you remember when you need the parens.
 * That said, the rules are:
 * </p>
 *
 * <p>
 * 1. Although you don't always need them, it is recommended style to always put parentheses
 * around right-hand values, such as the <code>7</code> in <code>num must equal (7)</code>:
 * </p>
 *
 * <pre class="indent">
 * result must equal <span style="color: #CC3300; font-weight: bold">(</span>4<span style="color: #CC3300; font-weight: bold">)</span>
 * array must have length <span style="color: #CC3300; font-weight: bold">(</span>3<span style="color: #CC3300; font-weight: bold">)</span>
 * book must have (
 *   'title <span style="color: #CC3300; font-weight: bold">(</span>"Programming in Scala"<span style="color: #CC3300; font-weight: bold">)</span>,
 *   'author <span style="color: #CC3300; font-weight: bold">(</span>List("Odersky", "Spoon", "Venners")<span style="color: #CC3300; font-weight: bold">)</span>,
 *   'pubYear <span style="color: #CC3300; font-weight: bold">(</span>2008<span style="color: #CC3300; font-weight: bold">)</span>
 * )
 * option must be <span style="color: #CC3300; font-weight: bold">(</span>'defined<span style="color: #CC3300; font-weight: bold">)</span>
 * catMap must (contain key <span style="color: #CC3300; font-weight: bold">(</span>9<span style="color: #CC3300; font-weight: bold">)</span> and contain value <span style="color: #CC3300; font-weight: bold">(</span>"lives"<span style="color: #CC3300; font-weight: bold">)</span>)</span>
 * keyEvent must be an <span style="color: #CC3300; font-weight: bold">(</span>'actionKey<span style="color: #CC3300; font-weight: bold">)</span>
 * javaSet must have size <span style="color: #CC3300; font-weight: bold">(</span>90<span style="color: #CC3300; font-weight: bold">)</span>
 * </pre>
 *
 * <p>
 * 2. Except for <code>length</code> and <code>size</code>, you must always put parentheses around
 * the list of one or more property values following a <code>have</code>:
 * </p>
 *
 * <pre class="indent">
 * file must (exist and have <span style="color: #CC3300; font-weight: bold">(</span>'name ("temp.txt")<span style="color: #CC3300; font-weight: bold">)</span>)
 * book must have <span style="color: #CC3300; font-weight: bold">(</span>
 *   title ("Programming in Scala"),
 *   author (List("Odersky", "Spoon", "Venners")),
 *   pubYear (2008)
 * <span style="color: #CC3300; font-weight: bold">)</span>
 * javaList must have length (9) // parens optional for length and size
 * </pre>
 *
 * <p>
 * 3. You must always put parentheses around <code>and</code> and <code>or</code> expressions, as in:
 * </p>
 *
 * <pre class="indent">
 * catMap must <span style="color: #CC3300; font-weight: bold">(</span>contain key (9) and contain value ("lives")<span style="color: #CC3300; font-weight: bold">)</span>
 * number must <span style="color: #CC3300; font-weight: bold">(</span>equal (2) or equal (4) or equal (8)<span style="color: #CC3300; font-weight: bold">)</span>
 * </pre>
 * 
 * <p>
 * 4. Although you don't always need them, it is recommended style to always put parentheses
 * around custom <code>Matcher</code>s when they appear directly after <code>not</code>:
 * </p>
 * 
 * <pre class="indent">
 * file must exist
 * file must not <span style="color: #CC3300; font-weight: bold">(</span>exist<span style="color: #CC3300; font-weight: bold">)</span>
 * file must (exist and have ('name ("temp.txt")))
 * file must (not <span style="color: #CC3300; font-weight: bold">(</span>exist<span style="color: #CC3300; font-weight: bold">)</span> and have ('name ("temp.txt"))
 * file must (have ('name ("temp.txt") or exist)
 * file must (have ('name ("temp.txt") or not <span style="color: #CC3300; font-weight: bold">(</span>exist<span style="color: #CC3300; font-weight: bold">)</span>)
 * </pre>
 *
 * <p>
 * That's it. With a bit of practice it  should become natural to you, and the compiler will always be there to tell you if you
 * forget a set of needed parentheses.
 * </p>
 */
trait MustMatchers extends Matchers with MustVerb {

  private object MustMethodHelper {
    def mustMatcher[T](left: T, rightMatcher: Matcher[T]) {
      rightMatcher(left) match {
        case MatchResult(false, failureMessage, _, _, _) => throw newTestFailedException(failureMessage)
        case _ => ()
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>Any</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class AnyMustWrapper[T](left: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must equal (3)
     *        ^
     * </pre>
     */
    def must(rightMatcher: Matcher[T]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * result must not equal (3)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord) = new ResultOfNotWord[T](left, false)
  }

  // I think the type hasn't been converted yet here. It is just a pass-through. It finally gets
  // converted in ResultOfHaveWordForLengthWrapper, at which point the actual implicit conversions
  // from String, Array, and the structural types get applied.
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>AnyRef</code> objects that can be implicitly converted to type <code>LengthWrapper</code>.
   * Trait <code>MustMatchers</code> includes implicit conversions from several types, including many structural types, to <code>LengthWrapper</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class LengthMustWrapper[A <: AnyRef <% LengthWrapper](left: A) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * objectWithLength must equal (3)
     *                  ^
     * </pre>
     */
    def must(rightMatcher: Matcher[A]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must have length (3)
     *        ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForLengthWrapper[A] =
      new ResultOfHaveWordForLengthWrapper(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must not have length (3)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForLengthWrapper[A] =
      new ResultOfNotWordForLengthWrapper(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must be theSameInstanceAs anotherObject
     *        ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[A] = new ResultOfBeWordForAnyRef[A](left, true)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>AnyRef</code> objects that can be implicitly converted to type <code>SizeWrapper</code>.
   * Trait <code>MustMatchers</code> includes implicit conversions from several types, including many structural types, to <code>SizeWrapper</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class SizeMustWrapper[A <: AnyRef <% SizeWrapper](left: A) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * objectWithSize must equal (3)
     *                ^
     * </pre>
     */
    def must(rightMatcher: Matcher[A]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must not have size (3)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForSizeWrapper[A] =
      new ResultOfNotWordForSizeWrapper(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must have size (3)
     *        ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForSizeWrapper[A] =
      new ResultOfHaveWordForSizeWrapper(left, true)

    // TODO I just added this. Didn't do a test for it.
    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must be theSameInstanceAs anotherObject
     *        ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[A] = new ResultOfBeWordForAnyRef[A](left, true)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>String</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class StringMustWrapper(left: String) extends StringMustWrapperForVerb(left) {

    /* *
     * This method enables syntax such as the following in a <code>FlatSpec</code>:
     *
     * <pre>
     * "A Stack (when empty)" must "be empty" in {
     *   assert(emptyStack.empty)
     * }
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> passes in a function via the implicit parameter that takes
     * three strings and results in a <code>ResultOfStringPassedToVerb</code>. This method
     * simply invokes this function, passing in left, right, and the verb string
     * <code>"must"</code>.
     * </p>
     *
    def must(right: String)(implicit fun: (String, String, String) => ResultOfStringPassedToVerb): ResultOfStringPassedToVerb = {
      fun(left, right, "must")
    }

    def must(right: => Unit)(implicit fun: (String, () => Unit, String) => Unit) {
      fun(left, right _, "must")
    }     */

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must equal ("hi")
     *        ^
     * </pre>
     */
    def must(rightMatcher: Matcher[String]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must be theSameInstanceAs anotherObject
     *        ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[String] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must have length (3)
     *        ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForString = {
      new ResultOfHaveWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must include regex ("hi")
     *        ^
     * </pre>
     */
    def must(includeWord: IncludeWord): ResultOfIncludeWordForString = {
      new ResultOfIncludeWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must startWith regex ("hello")
     *        ^
     * </pre>
     */
    def must(startWithWord: StartWithWord): ResultOfStartWithWordForString = {
      new ResultOfStartWithWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must endWith regex ("world")
     *        ^
     * </pre>
     */
    def must(endWithWord: EndWithWord): ResultOfEndWithWordForString = {
      new ResultOfEndWithWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must 
     *        ^
     * </pre>
     */
    def must(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = {
      new ResultOfFullyMatchWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * string must not have length (3)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForString = {
      new ResultOfNotWordForString(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>Double</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class DoubleMustWrapper(left: Double) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * aDouble must equal (8.8)
     *        ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Double]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * result must not equal (8.8)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForDouble = {
      new ResultOfNotWordForDouble(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>Float</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class FloatMustWrapper(left: Float) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * aFloat must equal (3.3f)
     *       ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Float]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * result must not equal (8.8f)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForFloat = {
      new ResultOfNotWordForFloat(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>Long</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class LongMustWrapper(left: Long) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * aLong must equal (3L)
     *      ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Long]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * result must not equal (88L)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForLong = {
      new ResultOfNotWordForLong(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>Int</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class IntMustWrapper(left: Int) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * anInt must equal (3)
     *      ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Int]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * result must not equal (8)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForInt = {
      new ResultOfNotWordForInt(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>Short</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class ShortMustWrapper(left: Short) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * aShort must equal (3.toShort)
     *        ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Short]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * result must not equal (8.toShort)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForShort = {
      new ResultOfNotWordForShort(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>Byte</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class ByteMustWrapper(left: Byte) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * aByte must equal (3.toByte)
     *       ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Byte]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * result must not equal (8.toByte)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForByte = {
      new ResultOfNotWordForByte(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>scala.collection.Map[K, V]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class MapMustWrapper[K, V](left: scala.collection.Map[K, V]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * map must equal (Map(1 -> "one", 2 -> "two"))
     *     ^
     * </pre>
     */
    def must(rightMatcher: Matcher[scala.collection.Map[K, V]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * map must be theSameInstanceAs (anotherMap)
     *     ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[scala.collection.Map[K, V]] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * map must have size (3)
     *     ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForCollection[(K, V)] = {
      new ResultOfHaveWordForCollection(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * map must contain key (10)
     *     ^
     * </pre>
     */
    def must(containWord: ContainWord): ResultOfContainWordForMap[K, V] = {
      new ResultOfContainWordForMap(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * map must not have size (3)
     *     ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForMap[K, V] = {
      new ResultOfNotWordForMap(left, false)
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>AnyRef</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class AnyRefMustWrapper[T <: AnyRef](left: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * anyRef must equal (anotherObject)
     *        ^
     * </pre>
     */
    def must(rightMatcher: Matcher[T]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must not have length (3)
     *        ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForAnyRef[T] =
      new ResultOfNotWordForAnyRef(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * object must be theSameInstanceAs anotherObject
     *        ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[T] = new ResultOfBeWordForAnyRef(left, true)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>scala.Collection[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class CollectionMustWrapper[T](left: Collection[T]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * collection must equal (Set(1, 2, 3))
     *            ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Collection[T]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * collection must have length (3)
     *            ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForCollection[T] =
      new ResultOfHaveWordForCollection(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * collection must be theSameInstanceAs anotherObject
     *            ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[Collection[T]] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * collection must not have size (3)
     *            ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForCollection[T, Collection[T]] =
      new ResultOfNotWordForCollection(left, false)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>java.util.Collection[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class JavaCollectionMustWrapper[T](left: java.util.Collection[T]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaCollection must equal (aJavaSet)
     *                ^
     * </pre>
     */
    def must(rightMatcher: Matcher[java.util.Collection[T]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaCollection must have size (3)
     *                ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForJavaCollection[T] =
      new ResultOfHaveWordForJavaCollection(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaCollection must be theSameInstanceAs anotherObject
     *                ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[java.util.Collection[T]] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaCollection must not have size (3)
     *                ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForJavaCollection[T, java.util.Collection[T]] =
      new ResultOfNotWordForJavaCollection(left, false)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>java.util.Map[K, V]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class JavaMapMustWrapper[K, V](left: java.util.Map[K, V]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaMap must equal (someJavaMap)
     *         ^
     * </pre>
     */
    def must(rightMatcher: Matcher[java.util.Map[K, V]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaMap must contain value (3)
     *         ^
     * </pre>
     */
    def must(containWord: ContainWord): ResultOfContainWordForJavaMap[K, V] = {
      new ResultOfContainWordForJavaMap(left, true)
    }
 
    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaMap must have size (3)
     *         ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForJavaMap = {
      new ResultOfHaveWordForJavaMap(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaMap must not have length (3)
     *         ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForJavaMap[K, V] = {
      new ResultOfNotWordForJavaMap[K, V](left, false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaMap must be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[java.util.Map[K, V]] = new ResultOfBeWordForAnyRef(left, true)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>scala.Seq[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class SeqMustWrapper[T](left: Seq[T]) {
 
    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * seq must equal (List(1, 2, 3))
     *     ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Seq[T]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * seq must have length (3)
     *     ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForSeq[T] =
      new ResultOfHaveWordForSeq(left, true)

/* TODO: This is what I think it must be. But it was the AnyRef one, or maybe even not that.
    def must(notWord: NotWord): ResultOfNotWordForSeq[T, List[T]] =
      new ResultOfNotWordForSeq(left, false)
*/
    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * seq must not have length (3)
     *     ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForAnyRef[Seq[T]] =
      new ResultOfNotWordForAnyRef(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * seq must be theSameInstanceAs anotherObject
     *     ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[Seq[T]] = new ResultOfBeWordForAnyRef(left, true)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>scala.Array[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class ArrayMustWrapper[T](left: Array[T]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * array must equal (Array("one", "two"))
     *       ^
     * </pre>
     */
    def must(rightMatcher: Matcher[Array[T]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * array must have length (3)
     *       ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForSeq[T] = {
      new ResultOfHaveWordForSeq(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * array must not have length (3)
     *       ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForSeq[T, Array[T]] =
      new ResultOfNotWordForSeq(left, false)
  }
  // Note, no must(beWord) is needed here because a different implicit conversion will be used
  // on "array shoudl be ..." because this one doesn't solve the type error.

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>scala.List[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class ListMustWrapper[T](left: List[T]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * list must equal (List(1, 2, 3))
     *      ^
     * </pre>
     */
    def must(rightMatcher: Matcher[List[T]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * list must be theSameInstanceAs anotherObject
     *      ^
     * </pre>
     */
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[List[T]] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * list must have length (3)
     *      ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForSeq[T] =
      new ResultOfHaveWordForSeq(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * list must not have length (3)
     *      ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForSeq[T, List[T]] =
      new ResultOfNotWordForSeq(left, false)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="MustMatchers.html"><code>MustMatchers</code></a> or <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on objects of type <code>java.util.List[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  class JavaListMustWrapper[T](left: java.util.List[T]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaList must equal (someOtherJavaList)
     *          ^
     * </pre>
     */
    def must(rightMatcher: Matcher[java.util.List[T]]) {
      MustMethodHelper.mustMatcher(left, rightMatcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaList must have length (3)
     *          ^
     * </pre>
     */
    def must(haveWord: HaveWord): ResultOfHaveWordForJavaList[T] = {
      new ResultOfHaveWordForJavaList(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre>
     * javaList must not have length (3)
     *          ^
     * </pre>
     */
    def must(notWord: NotWord): ResultOfNotWordForJavaList[T, java.util.List[T]] = {
      new ResultOfNotWordForJavaList(left, false)
    }
  }

  /**
   * Implicitly converts an object of type <code>T</code> to a <code>AnyMustWrapper[T]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToAnyMustWrapper[T](o: T): AnyMustWrapper[T] = new AnyMustWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.Double</code> to a <code>DoubleMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToDoubleMustWrapper(o: Double): DoubleMustWrapper = new DoubleMustWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.Float</code> to a <code>FloatMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToFloatMustWrapper(o: Float): FloatMustWrapper = new FloatMustWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.Long</code> to a <code>LongMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToLongMustWrapper(o: Long): LongMustWrapper = new LongMustWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.Int</code> to a <code>IntMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToIntMustWrapper(o: Int): IntMustWrapper = new IntMustWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.Short</code> to a <code>ShortMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToShortMustWrapper(o: Short): ShortMustWrapper = new ShortMustWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.Byte</code> to a <code>ByteMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToByteMustWrapper(o: Byte): ByteMustWrapper = new ByteMustWrapper(o)

  /**
   * Implicitly converts a <code>scala.AnyRef</code> of type <code>T</code> to an <code>AnyRefMustWrapper[T]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToAnyRefMustWrapper[T <: AnyRef](o: T): AnyRefMustWrapper[T] = new AnyRefMustWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.Collection[T]</code> to a <code>CollectionMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToCollectionMustWrapper[T](o: Collection[T]): CollectionMustWrapper[T] = new CollectionMustWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.Seq[T]</code> to a <code>SeqMustWrapper[T]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToSeqMustWrapper[T](o: Seq[T]): SeqMustWrapper[T] = new SeqMustWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.Array[T]</code> to a <code>ArrayMustWrapper[T]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToArrayMustWrapper[T](o: Array[T]): ArrayMustWrapper[T] = new ArrayMustWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.List[T]</code> to a <code>ListMustWrapper[T]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToListMustWrapper[T](o: List[T]): ListMustWrapper[T] = new ListMustWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.collection.Map[K, V]</code> to a <code>MapMustWrapper[K, V]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToMapMustWrapper[K, V](o: scala.collection.Map[K, V]): MapMustWrapper[K, V] = new MapMustWrapper[K, V](o)

  /**
   * Implicitly converts an object of type <code>java.lang.String</code> to a <code>StringMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit override def convertToStringMustWrapper(o: String): StringMustWrapper = new StringMustWrapper(o)

  /**
   * Implicitly converts an object of type <code>java.util.Collection[T]</code> to a <code>JavaCollectionMustWrapper[T]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToJavaCollectionMustWrapper[T](o: java.util.Collection[T]): JavaCollectionMustWrapper[T] = new JavaCollectionMustWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>java.util.List[T]</code> to a <code>JavaListMustWrapper[T]</code>,
   * to enable <code>must</code> methods to be invokable on that object. This conversion is necessary to enable
   * <code>length</code> to be used on Java <code>List</code>s.
   */
  implicit def convertToJavaListMustWrapper[T](o: java.util.List[T]): JavaListMustWrapper[T] = new JavaListMustWrapper[T](o)


  /**
   * Implicitly converts an object of type <code>java.util.Map[K, V]</code> to a <code>JavaMapMustWrapper[K, V]</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToJavaMapMustWrapper[K, V](o: java.util.Map[K, V]): JavaMapMustWrapper[K, V] = new JavaMapMustWrapper[K, V](o)

  // Implicitly just used to trigger the addition of the must method. The LengthMustWrapper
  // doesn't actually convert them, just passes it through. The conversion that happens here is to LengthMustWrapper,
  // and later, inside ResultOfHaveWordForLengthWrapper, the implicit conversion from T to LengthWrapper takes place. So
  // weirdly enough, here strings are treated structurally for the implicit that adds the must, but later they are
  // treated nominally by the implicit conversion from plain old String to StringLengthWrapper. So when length is
  // ultimately invoked up in ResultOfHaveWordForLengthWrapper, it is done directly, not with reflection. That's my
  // theory anyway.

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getLength</code> method that results in <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntGetLengthMethodToLengthMustWrapper[T <: AnyRef { def getLength(): Int}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getLength</code> <code>val</code> of type <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntGetLengthFieldToLengthMustWrapper[T <: AnyRef { val getLength: Int}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>length</code> <code>val</code> of type <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntLengthFieldToLengthMustWrapper[T <: AnyRef { val length: Int}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>length</code> method that results in <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntLengthMethodToLengthMustWrapper[T <: AnyRef { def length(): Int}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)


  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getLength</code> method that results in <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongGetLengthMethodToLengthMustWrapper[T <: AnyRef { def getLength(): Long}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getLength</code> <code>val</code> of type <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongGetLengthFieldToLengthMustWrapper[T <: AnyRef { val getLength: Long}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>length</code> <code>val</code> of type <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongLengthFieldToLengthMustWrapper[T <: AnyRef { val length: Long}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>length</code> method that results in <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongLengthMethodToLengthMustWrapper[T <: AnyRef { def length(): Long}](o: T): LengthMustWrapper[T] = new LengthMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getSize</code> method that results in <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntGetSizeMethodToSizeMustWrapper[T <: AnyRef { def getSize(): Int}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getSize</code> <code>val</code> of type <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntGetSizeFieldToSizeMustWrapper[T <: AnyRef { val getSize: Int}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>size</code> <code>val</code> of type <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntSizeFieldToSizeMustWrapper[T <: AnyRef { val size: Int}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>size</code> method that results in <code>Int</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasIntSizeMethodToSizeMustWrapper[T <: AnyRef { def size(): Int}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)


  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getSize</code> method that results in <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongGetSizeMethodToSizeMustWrapper[T <: AnyRef { def getSize(): Long}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>getSize</code> <code>val</code> of type <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongGetSizeFieldToSizeMustWrapper[T <: AnyRef { val getSize: Long}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>size</code> <code>val</code> type <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongSizeFieldToSizeMustWrapper[T <: AnyRef { val size: Long}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)

  /**
   * Implicitly converts an <code>AnyRef</code> of type <code>T</code> whose structure includes
   * a <code>size</code> method that results in <code>Long</code>
   * to a <code>SizeMustWrapper[T]</code>, to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertHasLongSizeMethodToSizeMustWrapper[T <: AnyRef { def size(): Long}](o: T): SizeMustWrapper[T] = new SizeMustWrapper[T](o)
}
/*
When Scala must chose between an implicit with a structural type and one with a nominal one,
the nominal one wins.

scala> set.size
res0: Int = 3

scala> class SetWrapper(payload: Set[Int]) { def prove() { println("SetWrapper") }}
defined class SetWrapper

scala> class SizeWrapper(payload: { def size: Int }) { def prove() { println("SizeWrapper") }}
defined class SizeWrapper

scala> new SizeWrapper(set)
res1: SizeWrapper = SizeWrapper@39ce9b

scala> res1.prove
SizeWrapper

scala> new SetWrapper(set)
res3: SetWrapper = SetWrapper@9fc9fe

scala> res3.prove
SetWrapper

scala> implicit def convertToSetWrapper(setParam: Set[Int]): SetWrapper = new SetWrapper(setParam)
convertToSetWrapper: (Set[Int])SetWrapper

scala> implicit def convertToSizeWrapper(setParam: { def size: Int }): SizeWrapper = new SizeWrapper(setParam)
convertToSizeWrapper: (AnyRef{def size: Int})SizeWrapper

scala> convertToSetWrapper(set)
res5: SetWrapper = SetWrapper@598095

scala> convertToSizeWrapper(set)
res6: SizeWrapper = SizeWrapper@660ff1

scala> set.prove
SetWrapper
 */
/*
leave this explanation in. It is a useful reminder.
THIS DOESN'T WORK BECAUSE...
  trait MustMethods[T] {
    val leftOperand: T
    def must(rightMatcher: Matcher[T]) {
      rightMatcher(leftOperand) match {
        case MatchResult(false, failureMessage, _) => throw newTestFailedException(failureMessage)
        case _ => ()
      }
    }

    // I don't think there's a be on Any, because a (symbol) and an (symbol), pluse
    // theSameInstanceAs only work on AnyRefs
    // def must(beWord: BeWord): ResultOfBeWord[T] = new ResultOfBeWord(leftOperand, true)
    def must(notWord: NotWord) = new ResultOfNotWord[T](leftOperand, false)
  }
  trait MustMethodsForAnyRef[T <: AnyRef] extends MustMethods[T] {
    val leftOperand: T
    override def must(notWord: NotWord): ResultOfNotWordForAnyRef[T] = {
      new ResultOfNotWordForAnyRef(leftOperand, false)
    }
    def must(beWord: BeWord): ResultOfBeWordForAnyRef[T] = new ResultOfBeWordForAnyRef[T](leftOperand, true)
  }

  class CollectionMustWrapper[T](left: Collection[T]) extends { val leftOperand = left }
  with MustMethodsForAnyRef[Collection[T]]
      with MustContainWordForIterableMethods[T] with MustHaveWordForCollectionMethods[T] {

    override def must(notWord: NotWord): ResultOfNotWordForCollection[Collection[T]] = {
      new ResultOfNotWordForCollection(leftOperand, false)
    }
  }
When you mix in the latter, the result type of must(BeWord) is still the more generic ResultOfNotWord, not ResultOfNotWordForAnyRef.
As a result it doesn't have an "a (Symbol)" method on it. This triggers another implicit conversion in this case:

emptySet must be a ('empty)

Turns into:

BeSymbolSpec.this.convertToAnyRefMustWrapper[BeSymbolSpec.this.CollectionMustWrapper[T]]
(BeSymbolSpec.this.convertToCollectionMustWrapper[T](emptySet)).must(BeSymbolSpec.this.be).
a(scala.Symbol.apply("empty"));

So the problem with having these "methods" traits extend each other is the covariant result
types don't get more specific visibly enough.

LATER: Well, I'm wondering if now that I've removed the be method in MustMethods if this will work. 
*/

/**
 * Companion object that facilitates the importing of <code>MustMatchers</code> members as 
 * an alternative to mixing it the trait. One use case is to import <code>MustMatchers</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre>
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.7.3.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * 
 * scala> import org.scalatest.matchers.MustMatchers._
 * import org.scalatest.matchers.MustMatchers._
 * 
 * scala> 1 must equal (2)
 * org.scalatest.TestFailedException: 1 did not equal 2
 * 	at org.scalatest.matchers.Helper$.newTestFailedException(Matchers.scala:40)
 * 	at org.scalatest.matchers.MustMatchers$MustMethodHelper$.mustMatcher(MustMatchers.scala:826)
 * 	at org.scalatest.matchers.MustMatchers$IntMustWrapper.must(MustMatchers.scala:1123)
 * 	at .<init>(<console>:9)
 * 	at .<clinit>(<console>)
 * 	at RequestR...
 *
 * scala> "hello, world" must startWith ("hello")
 * 
 * scala> 7 must (be >= (3) and not be <= (7))
 * org.scalatest.TestFailedException: 7 was greater than or equal to 3, but 7 was less than or equal to 7
 * 	at org.scalatest.matchers.Helper$.newTestFailedException(Matchers.scala:40)
 * 	at org.scalatest.matchers.MustMatchers$MustMethodHelper$.mustMatcher(MustMatchers.scala:826)
 * 	at org.scalatest.matchers.MustMatchers$IntMustWrapper.must(MustMatchers.scala:1123)
 * 	at .<init>(...
 * <pre>
 *
 * @author Bill Venners
 */
object MustMatchers extends MustMatchers
