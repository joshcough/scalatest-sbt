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

class MustPlusOrMinusSpec extends Spec with MustMatchers {

  describe("The be (X plusOrMinus Y) syntax") {

    val sevenDotOh = 7.0
    val minusSevenDotOh = -7.0
    val sevenDotOhFloat = 7.0f
    val minusSevenDotOhFloat = -7.0f
    val sevenLong = 7L
    val minusSevenLong = -7L
    val sevenInt = 7
    val minusSevenInt = -7
    val sevenShort: Short = 7
    val minusSevenShort: Short = -7
    val sevenByte: Byte = 7
    val minusSevenByte: Byte = -7

    /*
      I decided that for X plusOrMinus Y, Y can be any numeric type that's implicitly
      convertible to X. So if X is Double, Y could be Double, Float, Long, Int, Short, Byte.
      If X is Long, Y could be Long, Int, Short, Byte. If X is Short, Y could be Short or Byte.
      And if X is Byte, Y must be Byte.
      minusSevenDotOhFloat must be (-6.8f plusOrMinus 0.2d)
    */
    it("must do nothing if the number is within the specified range") {

      // Double plusOrMinus Double
      sevenDotOh must be (7.1 plusOrMinus 0.2)
      sevenDotOh must be (6.9 plusOrMinus 0.2)
      sevenDotOh must be (7.0 plusOrMinus 0.2)
      sevenDotOh must be (7.2 plusOrMinus 0.2)
      sevenDotOh must be (6.8 plusOrMinus 0.2)
      minusSevenDotOh must be (-7.1 plusOrMinus 0.2)
      minusSevenDotOh must be (-6.9 plusOrMinus 0.2)
      minusSevenDotOh must be (-7.0 plusOrMinus 0.2)
      minusSevenDotOh must be (-7.2 plusOrMinus 0.2)
      minusSevenDotOh must be (-6.8 plusOrMinus 0.2)

      // Double plusOrMinus Float
      sevenDotOh must be (7.1 plusOrMinus 0.2f)
      sevenDotOh must be (6.9 plusOrMinus 0.2f)
      sevenDotOh must be (7.0 plusOrMinus 0.2f)
      sevenDotOh must be (7.2 plusOrMinus 0.2f)
      sevenDotOh must be (6.8 plusOrMinus 0.2f)
      minusSevenDotOh must be (-7.1 plusOrMinus 0.2f)
      minusSevenDotOh must be (-6.9 plusOrMinus 0.2f)
      minusSevenDotOh must be (-7.0 plusOrMinus 0.2f)
      minusSevenDotOh must be (-7.2 plusOrMinus 0.2f)
      minusSevenDotOh must be (-6.8 plusOrMinus 0.2f)

      // Double plusOrMinus Long
      sevenDotOh must be (7.1 plusOrMinus 2L)
      sevenDotOh must be (6.9 plusOrMinus 2L)
      sevenDotOh must be (7.0 plusOrMinus 2L)
      sevenDotOh must be (7.2 plusOrMinus 2L)
      sevenDotOh must be (6.8 plusOrMinus 2L)
      minusSevenDotOh must be (-7.1 plusOrMinus 2L)
      minusSevenDotOh must be (-6.9 plusOrMinus 2L)
      minusSevenDotOh must be (-7.0 plusOrMinus 2L)
      minusSevenDotOh must be (-7.2 plusOrMinus 2L)
      minusSevenDotOh must be (-6.8 plusOrMinus 2L)

      // Double plusOrMinus Int
      sevenDotOh must be (7.1 plusOrMinus 2)
      sevenDotOh must be (6.9 plusOrMinus 2)
      sevenDotOh must be (7.0 plusOrMinus 2)
      sevenDotOh must be (7.2 plusOrMinus 2)
      sevenDotOh must be (6.8 plusOrMinus 2)
      minusSevenDotOh must be (-7.1 plusOrMinus 2)
      minusSevenDotOh must be (-6.9 plusOrMinus 2)
      minusSevenDotOh must be (-7.0 plusOrMinus 2)
      minusSevenDotOh must be (-7.2 plusOrMinus 2)
      minusSevenDotOh must be (-6.8 plusOrMinus 2)

      // Double plusOrMinus Short
      sevenDotOh must be (7.1 plusOrMinus 2.toShort)
      sevenDotOh must be (6.9 plusOrMinus 2.toShort)
      sevenDotOh must be (7.0 plusOrMinus 2.toShort)
      sevenDotOh must be (7.2 plusOrMinus 2.toShort)
      sevenDotOh must be (6.8 plusOrMinus 2.toShort)
      minusSevenDotOh must be (-7.1 plusOrMinus 2.toShort)
      minusSevenDotOh must be (-6.9 plusOrMinus 2.toShort)
      minusSevenDotOh must be (-7.0 plusOrMinus 2.toShort)
      minusSevenDotOh must be (-7.2 plusOrMinus 2.toShort)
      minusSevenDotOh must be (-6.8 plusOrMinus 2.toShort)

      // Double plusOrMinus Byte
      sevenDotOh must be (7.1 plusOrMinus 2.toByte)
      sevenDotOh must be (6.9 plusOrMinus 2.toByte)
      sevenDotOh must be (7.0 plusOrMinus 2.toByte)
      sevenDotOh must be (7.2 plusOrMinus 2.toByte)
      sevenDotOh must be (6.8 plusOrMinus 2.toByte)
      minusSevenDotOh must be (-7.1 plusOrMinus 2.toByte)
      minusSevenDotOh must be (-6.9 plusOrMinus 2.toByte)
      minusSevenDotOh must be (-7.0 plusOrMinus 2.toByte)
      minusSevenDotOh must be (-7.2 plusOrMinus 2.toByte)
      minusSevenDotOh must be (-6.8 plusOrMinus 2.toByte)

      // Float plusOrMinus Float
      sevenDotOhFloat must be (7.1f plusOrMinus 0.2f)
      sevenDotOhFloat must be (6.9f plusOrMinus 0.2f)
      sevenDotOhFloat must be (7.0f plusOrMinus 0.2f)
      sevenDotOhFloat must be (7.2f plusOrMinus 0.2f)
      sevenDotOhFloat must be (6.8f plusOrMinus 0.2f)
      minusSevenDotOhFloat must be (-7.1f plusOrMinus 0.2f)
      minusSevenDotOhFloat must be (-6.9f plusOrMinus 0.2f)
      minusSevenDotOhFloat must be (-7.0f plusOrMinus 0.2f)
      minusSevenDotOhFloat must be (-7.2f plusOrMinus 0.2f)
      minusSevenDotOhFloat must be (-6.8f plusOrMinus 0.2f)

      // Float plusOrMinus Long
      sevenDotOhFloat must be (7.1f plusOrMinus 2L)
      sevenDotOhFloat must be (6.9f plusOrMinus 2L)
      sevenDotOhFloat must be (7.0f plusOrMinus 2L)
      sevenDotOhFloat must be (7.2f plusOrMinus 2L)
      sevenDotOhFloat must be (6.8f plusOrMinus 2L)
      minusSevenDotOhFloat must be (-7.1f plusOrMinus 2L)
      minusSevenDotOhFloat must be (-6.9f plusOrMinus 2L)
      minusSevenDotOhFloat must be (-7.0f plusOrMinus 2L)
      minusSevenDotOhFloat must be (-7.2f plusOrMinus 2L)
      minusSevenDotOhFloat must be (-6.8f plusOrMinus 2L)

      // Float plusOrMinus Int
      sevenDotOhFloat must be (7.1f plusOrMinus 2)
      sevenDotOhFloat must be (6.9f plusOrMinus 2)
      sevenDotOhFloat must be (7.0f plusOrMinus 2)
      sevenDotOhFloat must be (7.2f plusOrMinus 2)
      sevenDotOhFloat must be (6.8f plusOrMinus 2)
      minusSevenDotOhFloat must be (-7.1f plusOrMinus 2)
      minusSevenDotOhFloat must be (-6.9f plusOrMinus 2)
      minusSevenDotOhFloat must be (-7.0f plusOrMinus 2)
      minusSevenDotOhFloat must be (-7.2f plusOrMinus 2)
      minusSevenDotOhFloat must be (-6.8f plusOrMinus 2)

      // Float plusOrMinus Short
      sevenDotOhFloat must be (7.1f plusOrMinus 2.toShort)
      sevenDotOhFloat must be (6.9f plusOrMinus 2.toShort)
      sevenDotOhFloat must be (7.0f plusOrMinus 2.toShort)
      sevenDotOhFloat must be (7.2f plusOrMinus 2.toShort)
      sevenDotOhFloat must be (6.8f plusOrMinus 2.toShort)
      minusSevenDotOhFloat must be (-7.1f plusOrMinus 2.toShort)
      minusSevenDotOhFloat must be (-6.9f plusOrMinus 2.toShort)
      minusSevenDotOhFloat must be (-7.0f plusOrMinus 2.toShort)
      minusSevenDotOhFloat must be (-7.2f plusOrMinus 2.toShort)
      minusSevenDotOhFloat must be (-6.8f plusOrMinus 2.toShort)

      // Float plusOrMinus Byte
      sevenDotOhFloat must be (7.1f plusOrMinus 2.toByte)
      sevenDotOhFloat must be (6.9f plusOrMinus 2.toByte)
      sevenDotOhFloat must be (7.0f plusOrMinus 2.toByte)
      sevenDotOhFloat must be (7.2f plusOrMinus 2.toByte)
      sevenDotOhFloat must be (6.8f plusOrMinus 2.toByte)
      minusSevenDotOhFloat must be (-7.1f plusOrMinus 2.toByte)
      minusSevenDotOhFloat must be (-6.9f plusOrMinus 2.toByte)
      minusSevenDotOhFloat must be (-7.0f plusOrMinus 2.toByte)
      minusSevenDotOhFloat must be (-7.2f plusOrMinus 2.toByte)
      minusSevenDotOhFloat must be (-6.8f plusOrMinus 2.toByte)

      // Long plusOrMinus Long
      sevenLong must be (9L plusOrMinus 2L)
      sevenLong must be (8L plusOrMinus 2L)
      sevenLong must be (7L plusOrMinus 2L)
      sevenLong must be (6L plusOrMinus 2L)
      sevenLong must be (5L plusOrMinus 2L)
      minusSevenLong must be (-9L plusOrMinus 2L)
      minusSevenLong must be (-8L plusOrMinus 2L)
      minusSevenLong must be (-7L plusOrMinus 2L)
      minusSevenLong must be (-6L plusOrMinus 2L)
      minusSevenLong must be (-5L plusOrMinus 2L)

      // Long plusOrMinus Int
      sevenLong must be (9L plusOrMinus 2)
      sevenLong must be (8L plusOrMinus 2)
      sevenLong must be (7L plusOrMinus 2)
      sevenLong must be (6L plusOrMinus 2)
      sevenLong must be (5L plusOrMinus 2)
      minusSevenLong must be (-9L plusOrMinus 2)
      minusSevenLong must be (-8L plusOrMinus 2)
      minusSevenLong must be (-7L plusOrMinus 2)
      minusSevenLong must be (-6L plusOrMinus 2)
      minusSevenLong must be (-5L plusOrMinus 2)

      // Long plusOrMinus Short
      sevenLong must be (9L plusOrMinus 2.toShort)
      sevenLong must be (8L plusOrMinus 2.toShort)
      sevenLong must be (7L plusOrMinus 2.toShort)
      sevenLong must be (6L plusOrMinus 2.toShort)
      sevenLong must be (5L plusOrMinus 2.toShort)
      minusSevenLong must be (-9L plusOrMinus 2.toShort)
      minusSevenLong must be (-8L plusOrMinus 2.toShort)
      minusSevenLong must be (-7L plusOrMinus 2.toShort)
      minusSevenLong must be (-6L plusOrMinus 2.toShort)
      minusSevenLong must be (-5L plusOrMinus 2.toShort)

      // Long plusOrMinus Byte
      sevenLong must be (9L plusOrMinus 2.toByte)
      sevenLong must be (8L plusOrMinus 2.toByte)
      sevenLong must be (7L plusOrMinus 2.toByte)
      sevenLong must be (6L plusOrMinus 2.toByte)
      sevenLong must be (5L plusOrMinus 2.toByte)
      minusSevenLong must be (-9L plusOrMinus 2.toByte)
      minusSevenLong must be (-8L plusOrMinus 2.toByte)
      minusSevenLong must be (-7L plusOrMinus 2.toByte)
      minusSevenLong must be (-6L plusOrMinus 2.toByte)
      minusSevenLong must be (-5L plusOrMinus 2.toByte)

      // Int plusOrMinus Int
      sevenInt must be (9 plusOrMinus 2)
      sevenInt must be (8 plusOrMinus 2)
      sevenInt must be (7 plusOrMinus 2)
      sevenInt must be (6 plusOrMinus 2)
      sevenInt must be (5 plusOrMinus 2)
      minusSevenInt must be (-9 plusOrMinus 2)
      minusSevenInt must be (-8 plusOrMinus 2)
      minusSevenInt must be (-7 plusOrMinus 2)
      minusSevenInt must be (-6 plusOrMinus 2)
      minusSevenInt must be (-5 plusOrMinus 2)

      // Int plusOrMinus Short
      sevenInt must be (9 plusOrMinus 2.toShort)
      sevenInt must be (8 plusOrMinus 2.toShort)
      sevenInt must be (7 plusOrMinus 2.toShort)
      sevenInt must be (6 plusOrMinus 2.toShort)
      sevenInt must be (5 plusOrMinus 2.toShort)
      minusSevenInt must be (-9 plusOrMinus 2.toShort)
      minusSevenInt must be (-8 plusOrMinus 2.toShort)
      minusSevenInt must be (-7 plusOrMinus 2.toShort)
      minusSevenInt must be (-6 plusOrMinus 2.toShort)
      minusSevenInt must be (-5 plusOrMinus 2.toShort)

      // Int plusOrMinus Byte
      sevenInt must be (9 plusOrMinus 2.toByte)
      sevenInt must be (8 plusOrMinus 2.toByte)
      sevenInt must be (7 plusOrMinus 2.toByte)
      sevenInt must be (6 plusOrMinus 2.toByte)
      sevenInt must be (5 plusOrMinus 2.toByte)
      minusSevenInt must be (-9 plusOrMinus 2.toByte)
      minusSevenInt must be (-8 plusOrMinus 2.toByte)
      minusSevenInt must be (-7 plusOrMinus 2.toByte)
      minusSevenInt must be (-6 plusOrMinus 2.toByte)
      minusSevenInt must be (-5 plusOrMinus 2.toByte)

      // Short plusOrMinus Short
      sevenShort must be (9.toShort plusOrMinus 2.toShort)
      sevenShort must be (8.toShort plusOrMinus 2.toShort)
      sevenShort must be (7.toShort plusOrMinus 2.toShort)
      sevenShort must be (6.toShort plusOrMinus 2.toShort)
      sevenShort must be (5.toShort plusOrMinus 2.toShort)
      minusSevenShort must be ((-9).toShort plusOrMinus 2.toShort)
      minusSevenShort must be ((-8).toShort plusOrMinus 2.toShort)
      minusSevenShort must be ((-7).toShort plusOrMinus 2.toShort)
      minusSevenShort must be ((-6).toShort plusOrMinus 2.toShort)
      minusSevenShort must be ((-5).toShort plusOrMinus 2.toShort)

      // Short plusOrMinus Byte
      sevenShort must be (9.toShort plusOrMinus 2.toByte)
      sevenShort must be (8.toShort plusOrMinus 2.toByte)
      sevenShort must be (7.toShort plusOrMinus 2.toByte)
      sevenShort must be (6.toShort plusOrMinus 2.toByte)
      sevenShort must be (5.toShort plusOrMinus 2.toByte)
      minusSevenShort must be ((-9).toShort plusOrMinus 2.toByte)
      minusSevenShort must be ((-8).toShort plusOrMinus 2.toByte)
      minusSevenShort must be ((-7).toShort plusOrMinus 2.toByte)
      minusSevenShort must be ((-6).toShort plusOrMinus 2.toByte)
      minusSevenShort must be ((-5).toShort plusOrMinus 2.toByte)

      // Byte plusOrMinus Byte
      sevenByte must be (9.toByte plusOrMinus 2.toByte)
      sevenByte must be (8.toByte plusOrMinus 2.toByte)
      sevenByte must be (7.toByte plusOrMinus 2.toByte)
      sevenByte must be (6.toByte plusOrMinus 2.toByte)
      sevenByte must be (5.toByte plusOrMinus 2.toByte)
      minusSevenByte must be ((-9).toByte plusOrMinus 2.toByte)
      minusSevenByte must be ((-8).toByte plusOrMinus 2.toByte)
      minusSevenByte must be ((-7).toByte plusOrMinus 2.toByte)
      minusSevenByte must be ((-6).toByte plusOrMinus 2.toByte)
      minusSevenByte must be ((-5).toByte plusOrMinus 2.toByte)
    }

    it("must do nothing if the number is within the specified range, when used with not") {

      // Double plusOrMinus Double
      sevenDotOh must not { be (7.5 plusOrMinus 0.2) }
      sevenDotOh must not be (7.5 plusOrMinus 0.2)
      sevenDotOh must not be (6.5 plusOrMinus 0.2)
      minusSevenDotOh must not { be (-7.5 plusOrMinus 0.2) }
      minusSevenDotOh must not be (-7.5 plusOrMinus 0.2)
      minusSevenDotOh must not be (-6.5 plusOrMinus 0.2)

      // Double plusOrMinus Float
      sevenDotOh must not { be (7.5 plusOrMinus 0.2f) }
      sevenDotOh must not be (7.5 plusOrMinus 0.2f)
      sevenDotOh must not be (6.5 plusOrMinus 0.2f)
      minusSevenDotOh must not { be (-7.5 plusOrMinus 0.2f) }
      minusSevenDotOh must not be (-7.5 plusOrMinus 0.2f)
      minusSevenDotOh must not be (-6.5 plusOrMinus 0.2f)

      // Double plusOrMinus Long
      sevenDotOh must not { be (10.0 plusOrMinus 2L) }
      sevenDotOh must not be (4.0 plusOrMinus 2L)
      sevenDotOh must not be (9.1 plusOrMinus 2L)
      minusSevenDotOh must not { be (-10.0 plusOrMinus 2L) }
      minusSevenDotOh must not be (-4.0 plusOrMinus 2L)
      minusSevenDotOh must not be (-9.1 plusOrMinus 2L)

      // Double plusOrMinus Int
      sevenDotOh must not { be (10.0 plusOrMinus 2) }
      sevenDotOh must not be (4.0 plusOrMinus 2)
      sevenDotOh must not be (9.1 plusOrMinus 2)
      minusSevenDotOh must not { be (-10.0 plusOrMinus 2) }
      minusSevenDotOh must not be (-4.0 plusOrMinus 2)
      minusSevenDotOh must not be (-9.1 plusOrMinus 2)

      // Double plusOrMinus Short
      sevenDotOh must not { be (10.0 plusOrMinus 2.toShort) }
      sevenDotOh must not be (4.0 plusOrMinus 2.toShort)
      sevenDotOh must not be (9.1 plusOrMinus 2.toShort)
      minusSevenDotOh must not { be (-10.0 plusOrMinus 2.toShort) }
      minusSevenDotOh must not be (-4.0 plusOrMinus 2.toShort)
      minusSevenDotOh must not be (-9.1 plusOrMinus 2.toShort)

      // Double plusOrMinus Byte
      sevenDotOh must not { be (10.0 plusOrMinus 2.toByte) }
      sevenDotOh must not be (4.0 plusOrMinus 2.toByte)
      sevenDotOh must not be (9.1 plusOrMinus 2.toByte)
      minusSevenDotOh must not { be (-10.0 plusOrMinus 2.toByte) }
      minusSevenDotOh must not be (-4.0 plusOrMinus 2.toByte)
      minusSevenDotOh must not be (-9.1 plusOrMinus 2.toByte)

      // Float plusOrMinus Float
      sevenDotOhFloat must not { be (7.5f plusOrMinus 0.2f) }
      sevenDotOhFloat must not be (7.5f plusOrMinus 0.2f)
      sevenDotOhFloat must not be (6.5f plusOrMinus 0.2f)
      minusSevenDotOhFloat must not { be (-7.5f plusOrMinus 0.2f) }
      minusSevenDotOhFloat must not be (-7.5f plusOrMinus 0.2f)
      minusSevenDotOhFloat must not be (-6.5f plusOrMinus 0.2f)

      // Float plusOrMinus Long
      sevenDotOhFloat must not { be (10.0f plusOrMinus 2L) }
      sevenDotOhFloat must not be (4.0f plusOrMinus 2L)
      sevenDotOhFloat must not be (9.1f plusOrMinus 2L)
      minusSevenDotOhFloat must not { be (-10.0f plusOrMinus 2L) }
      minusSevenDotOhFloat must not be (-4.0f plusOrMinus 2L)
      minusSevenDotOhFloat must not be (-9.1f plusOrMinus 2L)

      // Float plusOrMinus Int
      sevenDotOhFloat must not { be (10.0f plusOrMinus 2) }
      sevenDotOhFloat must not be (4.0f plusOrMinus 2)
      sevenDotOhFloat must not be (9.1f plusOrMinus 2)
      minusSevenDotOhFloat must not { be (-10.0f plusOrMinus 2) }
      minusSevenDotOhFloat must not be (-4.0f plusOrMinus 2)
      minusSevenDotOhFloat must not be (-9.1f plusOrMinus 2)

      // Float plusOrMinus Short
      sevenDotOhFloat must not { be (10.0f plusOrMinus 2.toShort) }
      sevenDotOhFloat must not be (4.0f plusOrMinus 2.toShort)
      sevenDotOhFloat must not be (9.1f plusOrMinus 2.toShort)
      minusSevenDotOhFloat must not { be (-10.0f plusOrMinus 2.toShort) }
      minusSevenDotOhFloat must not be (-4.0f plusOrMinus 2.toShort)
      minusSevenDotOhFloat must not be (-9.1f plusOrMinus 2.toShort)

      // Float plusOrMinus Byte
      sevenDotOhFloat must not { be (10.0f plusOrMinus 2.toByte) }
      sevenDotOhFloat must not be (4.0f plusOrMinus 2.toByte)
      sevenDotOhFloat must not be (9.1f plusOrMinus 2.toByte)
      minusSevenDotOhFloat must not { be (-10.0f plusOrMinus 2.toByte) }
      minusSevenDotOhFloat must not be (-4.0f plusOrMinus 2.toByte)
      minusSevenDotOhFloat must not be (-9.1f plusOrMinus 2.toByte)

      // Long plusOrMinus Long
      sevenLong must not { be (10L plusOrMinus 2L) }
      sevenLong must not be (4L plusOrMinus 2L)
      sevenLong must not be (10L plusOrMinus 2L)
      minusSevenLong must not { be (-10L plusOrMinus 2L) }
      minusSevenLong must not be (-4L plusOrMinus 2L)
      minusSevenLong must not be (-10L plusOrMinus 2L)

      // Long plusOrMinus Int
      sevenLong must not { be (10L plusOrMinus 2) }
      sevenLong must not be (4L plusOrMinus 2)
      sevenLong must not be (10L plusOrMinus 2)
      minusSevenLong must not { be (-10L plusOrMinus 2) }
      minusSevenLong must not be (-4L plusOrMinus 2)
      minusSevenLong must not be (-10L plusOrMinus 2)

      // Long plusOrMinus Short
      sevenLong must not { be (10L plusOrMinus 2.toShort) }
      sevenLong must not be (4L plusOrMinus 2.toShort)
      sevenLong must not be (10L plusOrMinus 2.toShort)
      minusSevenLong must not { be (-10L plusOrMinus 2.toShort) }
      minusSevenLong must not be (-4L plusOrMinus 2.toShort)
      minusSevenLong must not be (-10L plusOrMinus 2.toShort)

      // Long plusOrMinus Byte
      sevenLong must not { be (10L plusOrMinus 2.toByte) }
      sevenLong must not be (4L plusOrMinus 2.toByte)
      sevenLong must not be (10L plusOrMinus 2.toByte)
      minusSevenLong must not { be (-10L plusOrMinus 2.toByte) }
      minusSevenLong must not be (-4L plusOrMinus 2.toByte)
      minusSevenLong must not be (-10L plusOrMinus 2.toByte)

      // Int plusOrMinus Int
      sevenInt must not { be (10 plusOrMinus 2) }
      sevenInt must not be (4 plusOrMinus 2)
      sevenInt must not be (10 plusOrMinus 2)
      minusSevenInt must not { be (-10 plusOrMinus 2) }
      minusSevenInt must not be (-4 plusOrMinus 2)
      minusSevenInt must not be (-10 plusOrMinus 2)

      // Int plusOrMinus Short
      sevenInt must not { be (10 plusOrMinus 2.toShort) }
      sevenInt must not be (4 plusOrMinus 2.toShort)
      sevenInt must not be (10 plusOrMinus 2.toShort)
      minusSevenInt must not { be (-10 plusOrMinus 2.toShort) }
      minusSevenInt must not be (-4 plusOrMinus 2.toShort)
      minusSevenInt must not be (-10 plusOrMinus 2.toShort)

      // Int plusOrMinus Byte
      sevenInt must not { be (10 plusOrMinus 2.toByte) }
      sevenInt must not be (4 plusOrMinus 2.toByte)
      sevenInt must not be (10 plusOrMinus 2.toByte)
      minusSevenInt must not { be (-10 plusOrMinus 2.toByte) }
      minusSevenInt must not be (-4 plusOrMinus 2.toByte)
      minusSevenInt must not be (-10 plusOrMinus 2.toByte)

      // Short plusOrMinus Short
      sevenShort must not { be (10.toShort plusOrMinus 2.toShort) }
      sevenShort must not be (4.toShort plusOrMinus 2.toShort)
      sevenShort must not be (10.toShort plusOrMinus 2.toShort)
      minusSevenShort must not { be ((-10).toShort plusOrMinus 2.toShort) }
      minusSevenShort must not be ((-4).toShort plusOrMinus 2.toShort)
      minusSevenShort must not be ((-10).toShort plusOrMinus 2.toShort)

      // Short plusOrMinus Byte
      sevenShort must not { be (10.toShort plusOrMinus 2.toByte) }
      sevenShort must not be (4.toShort plusOrMinus 2.toByte)
      sevenShort must not be (10.toShort plusOrMinus 2.toByte)
      minusSevenShort must not { be ((-10).toShort plusOrMinus 2.toByte) }
      minusSevenShort must not be ((-4).toShort plusOrMinus 2.toByte)
      minusSevenShort must not be ((-10).toShort plusOrMinus 2.toByte)

      // Byte plusOrMinus Byte
      sevenByte must not { be (10.toByte plusOrMinus 2.toByte) }
      sevenByte must not be (4.toByte plusOrMinus 2.toByte)
      sevenByte must not be (10.toByte plusOrMinus 2.toByte)
      minusSevenByte must not { be ((-10).toByte plusOrMinus 2.toByte) }
      minusSevenByte must not be ((-4).toByte plusOrMinus 2.toByte)
      minusSevenByte must not be ((-10).toByte plusOrMinus 2.toByte)
    }

    it("must do nothing if the number is within the specified range, when used in a logical-and expression") {

      // Double plusOrMinus Double
      sevenDotOh must ((be (7.1 plusOrMinus 0.2)) and (be (7.1 plusOrMinus 0.2)))
      sevenDotOh must (be (6.9 plusOrMinus 0.2) and (be (7.1 plusOrMinus 0.2)))
      sevenDotOh must (be (7.0 plusOrMinus 0.2) and be (7.0 plusOrMinus 0.2))

      // Double plusOrMinus Float
      sevenDotOh must ((be (7.1 plusOrMinus 0.2f)) and (be (7.1 plusOrMinus 0.2f)))
      sevenDotOh must (be (6.9 plusOrMinus 0.2f) and (be (7.1 plusOrMinus 0.2f)))
      sevenDotOh must (be (7.0 plusOrMinus 0.2f) and be (7.0 plusOrMinus 0.2f))

      // Double plusOrMinus Long
      sevenDotOh must ((be (7.1 plusOrMinus 2L)) and (be (7.1 plusOrMinus 2L)))
      sevenDotOh must (be (6.9 plusOrMinus 2L) and (be (7.1 plusOrMinus 2L)))
      sevenDotOh must (be (7.0 plusOrMinus 2L) and be (7.0 plusOrMinus 2L))

      // Double plusOrMinus Int
      sevenDotOh must ((be (7.1 plusOrMinus 2)) and (be (7.1 plusOrMinus 2)))
      sevenDotOh must (be (6.9 plusOrMinus 2) and (be (7.1 plusOrMinus 2)))
      sevenDotOh must (be (7.0 plusOrMinus 2) and be (7.0 plusOrMinus 2))

      // Double plusOrMinus Short
      sevenDotOh must ((be (7.1 plusOrMinus 2.toShort)) and (be (7.1 plusOrMinus 2.toShort)))
      sevenDotOh must (be (6.9 plusOrMinus 2.toShort) and (be (7.1 plusOrMinus 2.toShort)))
      sevenDotOh must (be (7.0 plusOrMinus 2.toShort) and be (7.0 plusOrMinus 2.toShort))

      // Double plusOrMinus Byte
      sevenDotOh must ((be (7.1 plusOrMinus 2.toByte)) and (be (7.1 plusOrMinus 2.toByte)))
      sevenDotOh must (be (6.9 plusOrMinus 2.toByte) and (be (7.1 plusOrMinus 2.toByte)))
      sevenDotOh must (be (7.0 plusOrMinus 2.toByte) and be (7.0 plusOrMinus 2.toByte))

      // Float plusOrMinus Float
      sevenDotOhFloat must ((be (7.1f plusOrMinus 0.2f)) and (be (7.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 0.2f) and (be (7.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 0.2f) and be (7.0f plusOrMinus 0.2f))

      // Float plusOrMinus Long
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2L)) and (be (7.1f plusOrMinus 2L)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2L) and (be (7.1f plusOrMinus 2L)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2L) and be (7.0f plusOrMinus 2L))

      // Float plusOrMinus Int
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2)) and (be (7.1f plusOrMinus 2)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2) and (be (7.1f plusOrMinus 2)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2) and be (7.0f plusOrMinus 2))

      // Float plusOrMinus Short
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2.toShort)) and (be (7.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2.toShort) and (be (7.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2.toShort) and be (7.0f plusOrMinus 2.toShort))

      // Float plusOrMinus Byte
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2.toByte)) and (be (7.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2.toByte) and (be (7.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2.toByte) and be (7.0f plusOrMinus 2.toByte))

      // Long plusOrMinus Long
      sevenLong must ((be (9L plusOrMinus 2L)) and (be (9L plusOrMinus 2L)))
      sevenLong must (be (8L plusOrMinus 2L) and (be (9L plusOrMinus 2L)))
      sevenLong must (be (7L plusOrMinus 2L) and be (7L plusOrMinus 2L))

      // Long plusOrMinus Int
      sevenLong must ((be (9L plusOrMinus 2)) and (be (9L plusOrMinus 2)))
      sevenLong must (be (8L plusOrMinus 2) and (be (9L plusOrMinus 2)))
      sevenLong must (be (7L plusOrMinus 2) and be (7L plusOrMinus 2))

      // Long plusOrMinus Short
      sevenLong must ((be (9L plusOrMinus 2.toShort)) and (be (9L plusOrMinus 2.toShort)))
      sevenLong must (be (8L plusOrMinus 2.toShort) and (be (9L plusOrMinus 2.toShort)))
      sevenLong must (be (7L plusOrMinus 2.toShort) and be (7L plusOrMinus 2.toShort))

      // Long plusOrMinus Byte
      sevenLong must ((be (9L plusOrMinus 2.toByte)) and (be (9L plusOrMinus 2.toByte)))
      sevenLong must (be (8L plusOrMinus 2.toByte) and (be (9L plusOrMinus 2.toByte)))
      sevenLong must (be (7L plusOrMinus 2.toByte) and be (7L plusOrMinus 2.toByte))

      // Int plusOrMinus Int
      sevenInt must ((be (9 plusOrMinus 2)) and (be (9 plusOrMinus 2)))
      sevenInt must (be (8 plusOrMinus 2) and (be (9 plusOrMinus 2)))
      sevenInt must (be (7 plusOrMinus 2) and be (7 plusOrMinus 2))

      // Int plusOrMinus Short
      sevenInt must ((be (9 plusOrMinus 2.toShort)) and (be (9 plusOrMinus 2.toShort)))
      sevenInt must (be (8 plusOrMinus 2.toShort) and (be (9 plusOrMinus 2.toShort)))
      sevenInt must (be (7 plusOrMinus 2.toShort) and be (7 plusOrMinus 2.toShort))

      // Int plusOrMinus Byte
      sevenInt must ((be (9 plusOrMinus 2.toByte)) and (be (9 plusOrMinus 2.toByte)))
      sevenInt must (be (8 plusOrMinus 2.toByte) and (be (9 plusOrMinus 2.toByte)))
      sevenInt must (be (7 plusOrMinus 2.toByte) and be (7 plusOrMinus 2.toByte))

      // Short plusOrMinus Short
      sevenShort must ((be (9.toShort plusOrMinus 2.toShort)) and (be (9.toShort plusOrMinus 2.toShort)))
      sevenShort must (be (8.toShort plusOrMinus 2.toShort) and (be (9.toShort plusOrMinus 2.toShort)))
      sevenShort must (be (7.toShort plusOrMinus 2.toShort) and be (7.toShort plusOrMinus 2.toShort))

      // Short plusOrMinus Byte
      sevenShort must ((be (9.toShort plusOrMinus 2.toByte)) and (be (9.toShort plusOrMinus 2.toByte)))
      sevenShort must (be (8.toShort plusOrMinus 2.toByte) and (be (9.toShort plusOrMinus 2.toByte)))
      sevenShort must (be (7.toShort plusOrMinus 2.toByte) and be (7.toShort plusOrMinus 2.toByte))

      // Byte plusOrMinus Byte
      sevenByte must ((be (9.toByte plusOrMinus 2.toByte)) and (be (9.toByte plusOrMinus 2.toByte)))
      sevenByte must (be (8.toByte plusOrMinus 2.toByte) and (be (9.toByte plusOrMinus 2.toByte)))
      sevenByte must (be (7.toByte plusOrMinus 2.toByte) and be (7.toByte plusOrMinus 2.toByte))
    }

    it("must do nothing if the number is within the specified range, when used in a logical-or expression") {

      // Double plusOrMinus Double
      sevenDotOh must ((be (7.1 plusOrMinus 0.2)) or (be (7.1 plusOrMinus 0.2)))
      sevenDotOh must (be (6.9 plusOrMinus 0.2) or (be (7.1 plusOrMinus 0.2)))
      sevenDotOh must (be (7.0 plusOrMinus 0.2) or be (7.0 plusOrMinus 0.2))

      // Double plusOrMinus Float
      sevenDotOh must ((be (7.1 plusOrMinus 0.2f)) or (be (7.1 plusOrMinus 0.2f)))
      sevenDotOh must (be (6.9 plusOrMinus 0.2f) or (be (7.1 plusOrMinus 0.2f)))
      sevenDotOh must (be (7.0 plusOrMinus 0.2f) or be (7.0 plusOrMinus 0.2f))

      // Double plusOrMinus Long
      sevenDotOh must ((be (7.1 plusOrMinus 2L)) or (be (7.1 plusOrMinus 2L)))
      sevenDotOh must (be (6.9 plusOrMinus 2L) or (be (7.1 plusOrMinus 2L)))
      sevenDotOh must (be (7.0 plusOrMinus 2L) or be (7.0 plusOrMinus 2L))

      // Double plusOrMinus Int
      sevenDotOh must ((be (7.1 plusOrMinus 2)) or (be (7.1 plusOrMinus 2)))
      sevenDotOh must (be (6.9 plusOrMinus 2) or (be (7.1 plusOrMinus 2)))
      sevenDotOh must (be (7.0 plusOrMinus 2) or be (7.0 plusOrMinus 2))

      // Double plusOrMinus Short
      sevenDotOh must ((be (7.1 plusOrMinus 2.toShort)) or (be (7.1 plusOrMinus 2.toShort)))
      sevenDotOh must (be (6.9 plusOrMinus 2.toShort) or (be (7.1 plusOrMinus 2.toShort)))
      sevenDotOh must (be (7.0 plusOrMinus 2.toShort) or be (7.0 plusOrMinus 2.toShort))

      // Double plusOrMinus Byte
      sevenDotOh must ((be (7.1 plusOrMinus 2.toByte)) or (be (7.1 plusOrMinus 2.toByte)))
      sevenDotOh must (be (6.9 plusOrMinus 2.toByte) or (be (7.1 plusOrMinus 2.toByte)))
      sevenDotOh must (be (7.0 plusOrMinus 2.toByte) or be (7.0 plusOrMinus 2.toByte))

      // Float plusOrMinus Float
      sevenDotOhFloat must ((be (7.1f plusOrMinus 0.2f)) or (be (7.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 0.2f) or (be (7.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 0.2f) or be (7.0f plusOrMinus 0.2f))

      // Float plusOrMinus Long
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2L)) or (be (7.1f plusOrMinus 2L)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2L) or (be (7.1f plusOrMinus 2L)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2L) or be (7.0f plusOrMinus 2L))

      // Float plusOrMinus Int
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2)) or (be (7.1f plusOrMinus 2)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2) or (be (7.1f plusOrMinus 2)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2) or be (7.0f plusOrMinus 2))

      // Float plusOrMinus Short
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2.toShort)) or (be (7.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2.toShort) or (be (7.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2.toShort) or be (7.0f plusOrMinus 2.toShort))

      // Float plusOrMinus Byte
      sevenDotOhFloat must ((be (7.1f plusOrMinus 2.toByte)) or (be (7.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (be (6.9f plusOrMinus 2.toByte) or (be (7.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (be (7.0f plusOrMinus 2.toByte) or be (7.0f plusOrMinus 2.toByte))

      // Long plusOrMinus Long
      sevenLong must ((be (9L plusOrMinus 2L)) or (be (9L plusOrMinus 2L)))
      sevenLong must (be (8L plusOrMinus 2L) or (be (9L plusOrMinus 2L)))
      sevenLong must (be (7L plusOrMinus 2L) or be (7L plusOrMinus 2L))

      // Long plusOrMinus Int
      sevenLong must ((be (9L plusOrMinus 2)) or (be (9L plusOrMinus 2)))
      sevenLong must (be (8L plusOrMinus 2) or (be (9L plusOrMinus 2)))
      sevenLong must (be (7L plusOrMinus 2) or be (7L plusOrMinus 2))

      // Long plusOrMinus Short
      sevenLong must ((be (9L plusOrMinus 2.toShort)) or (be (9L plusOrMinus 2.toShort)))
      sevenLong must (be (8L plusOrMinus 2.toShort) or (be (9L plusOrMinus 2.toShort)))
      sevenLong must (be (7L plusOrMinus 2.toShort) or be (7L plusOrMinus 2.toShort))

      // Long plusOrMinus Byte
      sevenLong must ((be (9L plusOrMinus 2.toByte)) or (be (9L plusOrMinus 2.toByte)))
      sevenLong must (be (8L plusOrMinus 2.toByte) or (be (9L plusOrMinus 2.toByte)))
      sevenLong must (be (7L plusOrMinus 2.toByte) or be (7L plusOrMinus 2.toByte))

      // Int plusOrMinus Int
      sevenInt must ((be (9 plusOrMinus 2)) or (be (9 plusOrMinus 2)))
      sevenInt must (be (8 plusOrMinus 2) or (be (9 plusOrMinus 2)))
      sevenInt must (be (7 plusOrMinus 2) or be (7 plusOrMinus 2))

      // Int plusOrMinus Short
      sevenInt must ((be (9 plusOrMinus 2.toShort)) or (be (9 plusOrMinus 2.toShort)))
      sevenInt must (be (8 plusOrMinus 2.toShort) or (be (9 plusOrMinus 2.toShort)))
      sevenInt must (be (7 plusOrMinus 2.toShort) or be (7 plusOrMinus 2.toShort))

      // Int plusOrMinus Byte
      sevenInt must ((be (9 plusOrMinus 2.toByte)) or (be (9 plusOrMinus 2.toByte)))
      sevenInt must (be (8 plusOrMinus 2.toByte) or (be (9 plusOrMinus 2.toByte)))
      sevenInt must (be (7 plusOrMinus 2.toByte) or be (7 plusOrMinus 2.toByte))

      // Short plusOrMinus Short
      sevenShort must ((be (9.toShort plusOrMinus 2.toShort)) or (be (9.toShort plusOrMinus 2.toShort)))
      sevenShort must (be (8.toShort plusOrMinus 2.toShort) or (be (9.toShort plusOrMinus 2.toShort)))
      sevenShort must (be (7.toShort plusOrMinus 2.toShort) or be (7.toShort plusOrMinus 2.toShort))

      // Short plusOrMinus Byte
      sevenShort must ((be (9.toShort plusOrMinus 2.toByte)) or (be (9.toShort plusOrMinus 2.toByte)))
      sevenShort must (be (8.toShort plusOrMinus 2.toByte) or (be (9.toShort plusOrMinus 2.toByte)))
      sevenShort must (be (7.toShort plusOrMinus 2.toByte) or be (7.toShort plusOrMinus 2.toByte))

      // Byte plusOrMinus Byte
      sevenByte must ((be (9.toByte plusOrMinus 2.toByte)) or (be (9.toByte plusOrMinus 2.toByte)))
      sevenByte must (be (8.toByte plusOrMinus 2.toByte) or (be (9.toByte plusOrMinus 2.toByte)))
      sevenByte must (be (7.toByte plusOrMinus 2.toByte) or be (7.toByte plusOrMinus 2.toByte))
    }

    it("must do nothing if the number is not within the specified range, when used in a logical-and expression with not") {

      // Double plusOrMinus Double
      sevenDotOh must ((not be (17.1 plusOrMinus 0.2)) and (not be (17.1 plusOrMinus 0.2)))
      sevenDotOh must (not (be (16.9 plusOrMinus 0.2)) and not (be (17.1 plusOrMinus 0.2)))
      sevenDotOh must (not be (17.0 plusOrMinus 0.2) and not be (17.0 plusOrMinus 0.2))

      // Double plusOrMinus Float
      sevenDotOh must ((not be (17.1 plusOrMinus 0.2f)) and (not be (17.1 plusOrMinus 0.2f)))
      sevenDotOh must (not (be (16.9 plusOrMinus 0.2f)) and not (be (17.1 plusOrMinus 0.2f)))
      sevenDotOh must (not be (17.0 plusOrMinus 0.2f) and not be (17.0 plusOrMinus 0.2f))

      // Double plusOrMinus Long
      sevenDotOh must ((not be (17.1 plusOrMinus 2L)) and (not be (17.1 plusOrMinus 2L)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2L)) and not (be (17.1 plusOrMinus 2L)))
      sevenDotOh must (not be (17.0 plusOrMinus 2L) and not be (17.0 plusOrMinus 2L))

      // Double plusOrMinus Int
      sevenDotOh must ((not be (17.1 plusOrMinus 2)) and (not be (17.1 plusOrMinus 2)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2)) and not (be (17.1 plusOrMinus 2)))
      sevenDotOh must (not be (17.0 plusOrMinus 2) and not be (17.0 plusOrMinus 2))

      // Double plusOrMinus Short
      sevenDotOh must ((not be (17.1 plusOrMinus 2.toShort)) and (not be (17.1 plusOrMinus 2.toShort)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2.toShort)) and not (be (17.1 plusOrMinus 2.toShort)))
      sevenDotOh must (not be (17.0 plusOrMinus 2.toShort) and not be (17.0 plusOrMinus 2.toShort))

      // Double plusOrMinus Byte
      sevenDotOh must ((not be (17.1 plusOrMinus 2.toByte)) and (not be (17.1 plusOrMinus 2.toByte)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2.toByte)) and not (be (17.1 plusOrMinus 2.toByte)))
      sevenDotOh must (not be (17.0 plusOrMinus 2.toByte) and not be (17.0 plusOrMinus 2.toByte))

      // Float plusOrMinus Float
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 0.2f)) and (not be (17.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 0.2f)) and not (be (17.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 0.2f) and not be (17.0f plusOrMinus 0.2f))

      // Float plusOrMinus Long
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2L)) and (not be (17.1f plusOrMinus 2L)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2L)) and not (be (17.1f plusOrMinus 2L)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2L) and not be (17.0f plusOrMinus 2L))

      // Float plusOrMinus Int
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2)) and (not be (17.1f plusOrMinus 2)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2)) and not (be (17.1f plusOrMinus 2)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2) and not be (17.0f plusOrMinus 2))

      // Float plusOrMinus Short
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2.toShort)) and (not be (17.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2.toShort)) and not (be (17.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2.toShort) and not be (17.0f plusOrMinus 2.toShort))

      // Float plusOrMinus Byte
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2.toByte)) and (not be (17.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2.toByte)) and not (be (17.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2.toByte) and not be (17.0f plusOrMinus 2.toByte))

      // Long plusOrMinus Long
      sevenLong must ((not be (19L plusOrMinus 2L)) and (not be (19L plusOrMinus 2L)))
      sevenLong must (not (be (18L plusOrMinus 2L)) and not (be (19L plusOrMinus 2L)))
      sevenLong must (not be (17L plusOrMinus 2L) and not be (17L plusOrMinus 2L))

      // Long plusOrMinus Int
      sevenLong must ((not be (19L plusOrMinus 2)) and (not be (19L plusOrMinus 2)))
      sevenLong must (not (be (18L plusOrMinus 2)) and not (be (19L plusOrMinus 2)))
      sevenLong must (not be (17L plusOrMinus 2) and not be (17L plusOrMinus 2))

      // Long plusOrMinus Short
      sevenLong must ((not be (19L plusOrMinus 2.toShort)) and (not be (19L plusOrMinus 2.toShort)))
      sevenLong must (not (be (18L plusOrMinus 2.toShort)) and not (be (19L plusOrMinus 2.toShort)))
      sevenLong must (not be (17L plusOrMinus 2.toShort) and not be (17L plusOrMinus 2.toShort))

      // Long plusOrMinus Byte
      sevenLong must ((not be (19L plusOrMinus 2.toByte)) and (not be (19L plusOrMinus 2.toByte)))
      sevenLong must (not (be (18L plusOrMinus 2.toByte)) and not (be (19L plusOrMinus 2.toByte)))
      sevenLong must (not be (17L plusOrMinus 2.toByte) and not be (17L plusOrMinus 2.toByte))

      // Int plusOrMinus Int
      sevenInt must ((not be (19 plusOrMinus 2)) and (not be (19 plusOrMinus 2)))
      sevenInt must (not (be (18 plusOrMinus 2)) and not (be (19 plusOrMinus 2)))
      sevenInt must (not be (17 plusOrMinus 2) and not be (17 plusOrMinus 2))

      // Int plusOrMinus Short
      sevenInt must ((not be (19 plusOrMinus 2.toShort)) and (not be (19 plusOrMinus 2.toShort)))
      sevenInt must (not (be (18 plusOrMinus 2.toShort)) and not (be (19 plusOrMinus 2.toShort)))
      sevenInt must (not be (17 plusOrMinus 2.toShort) and not be (17 plusOrMinus 2.toShort))

      // Int plusOrMinus Byte
      sevenInt must ((not be (19 plusOrMinus 2.toByte)) and (not be (19 plusOrMinus 2.toByte)))
      sevenInt must (not (be (18 plusOrMinus 2.toByte)) and not (be (19 plusOrMinus 2.toByte)))
      sevenInt must (not be (17 plusOrMinus 2.toByte) and not be (17 plusOrMinus 2.toByte))

      // Short plusOrMinus Short
      sevenShort must ((not be (19.toShort plusOrMinus 2.toShort)) and (not be (19.toShort plusOrMinus 2.toShort)))
      sevenShort must (not (be (18.toShort plusOrMinus 2.toShort)) and not (be (19.toShort plusOrMinus 2.toShort)))
      sevenShort must (not be (17.toShort plusOrMinus 2.toShort) and not be (17.toShort plusOrMinus 2.toShort))

      // Short plusOrMinus Byte
      sevenShort must ((not be (19.toShort plusOrMinus 2.toByte)) and (not be (19.toShort plusOrMinus 2.toByte)))
      sevenShort must (not (be (18.toShort plusOrMinus 2.toByte)) and not (be (19.toShort plusOrMinus 2.toByte)))
      sevenShort must (not be (17.toShort plusOrMinus 2.toByte) and not be (17.toShort plusOrMinus 2.toByte))

      // Byte plusOrMinus Byte
      sevenByte must ((not be (19.toByte plusOrMinus 2.toByte)) and (not be (19.toByte plusOrMinus 2.toByte)))
      sevenByte must (not (be (18.toByte plusOrMinus 2.toByte)) and not (be (19.toByte plusOrMinus 2.toByte)))
      sevenByte must (not be (17.toByte plusOrMinus 2.toByte) and not be (17.toByte plusOrMinus 2.toByte))
    }

    it("must do nothing if the number is not within the specified range, when used in a logical-or expression with not") {

      // Double plusOrMinus Double
      sevenDotOh must ((not be (17.1 plusOrMinus 0.2)) or (not be (17.1 plusOrMinus 0.2)))
      sevenDotOh must (not (be (16.9 plusOrMinus 0.2)) or not (be (17.1 plusOrMinus 0.2)))
      sevenDotOh must (not be (17.0 plusOrMinus 0.2) or not be (17.0 plusOrMinus 0.2))

      // Double plusOrMinus Float
      sevenDotOh must ((not be (17.1 plusOrMinus 0.2f)) or (not be (17.1 plusOrMinus 0.2f)))
      sevenDotOh must (not (be (16.9 plusOrMinus 0.2f)) or not (be (17.1 plusOrMinus 0.2f)))
      sevenDotOh must (not be (17.0 plusOrMinus 0.2f) or not be (17.0 plusOrMinus 0.2f))

      // Double plusOrMinus Long
      sevenDotOh must ((not be (17.1 plusOrMinus 2L)) or (not be (17.1 plusOrMinus 2L)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2L)) or not (be (17.1 plusOrMinus 2L)))
      sevenDotOh must (not be (17.0 plusOrMinus 2L) or not be (17.0 plusOrMinus 2L))

      // Double plusOrMinus Int
      sevenDotOh must ((not be (17.1 plusOrMinus 2)) or (not be (17.1 plusOrMinus 2)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2)) or not (be (17.1 plusOrMinus 2)))
      sevenDotOh must (not be (17.0 plusOrMinus 2) or not be (17.0 plusOrMinus 2))

      // Double plusOrMinus Short
      sevenDotOh must ((not be (17.1 plusOrMinus 2.toShort)) or (not be (17.1 plusOrMinus 2.toShort)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2.toShort)) or not (be (17.1 plusOrMinus 2.toShort)))
      sevenDotOh must (not be (17.0 plusOrMinus 2.toShort) or not be (17.0 plusOrMinus 2.toShort))

      // Double plusOrMinus Byte
      sevenDotOh must ((not be (17.1 plusOrMinus 2.toByte)) or (not be (17.1 plusOrMinus 2.toByte)))
      sevenDotOh must (not (be (16.9 plusOrMinus 2.toByte)) or not (be (17.1 plusOrMinus 2.toByte)))
      sevenDotOh must (not be (17.0 plusOrMinus 2.toByte) or not be (17.0 plusOrMinus 2.toByte))

      // Float plusOrMinus Float
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 0.2f)) or (not be (17.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 0.2f)) or not (be (17.1f plusOrMinus 0.2f)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 0.2f) or not be (17.0f plusOrMinus 0.2f))

      // Float plusOrMinus Long
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2L)) or (not be (17.1f plusOrMinus 2L)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2L)) or not (be (17.1f plusOrMinus 2L)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2L) or not be (17.0f plusOrMinus 2L))

      // Float plusOrMinus Int
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2)) or (not be (17.1f plusOrMinus 2)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2)) or not (be (17.1f plusOrMinus 2)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2) or not be (17.0f plusOrMinus 2))

      // Float plusOrMinus Short
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2.toShort)) or (not be (17.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2.toShort)) or not (be (17.1f plusOrMinus 2.toShort)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2.toShort) or not be (17.0f plusOrMinus 2.toShort))

      // Float plusOrMinus Byte
      sevenDotOhFloat must ((not be (17.1f plusOrMinus 2.toByte)) or (not be (17.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (not (be (16.9f plusOrMinus 2.toByte)) or not (be (17.1f plusOrMinus 2.toByte)))
      sevenDotOhFloat must (not be (17.0f plusOrMinus 2.toByte) or not be (17.0f plusOrMinus 2.toByte))

      // Long plusOrMinus Long
      sevenLong must ((not be (19L plusOrMinus 2L)) or (not be (19L plusOrMinus 2L)))
      sevenLong must (not (be (18L plusOrMinus 2L)) or not (be (19L plusOrMinus 2L)))
      sevenLong must (not be (17L plusOrMinus 2L) or not be (17L plusOrMinus 2L))

      // Long plusOrMinus Int
      sevenLong must ((not be (19L plusOrMinus 2)) or (not be (19L plusOrMinus 2)))
      sevenLong must (not (be (18L plusOrMinus 2)) or not (be (19L plusOrMinus 2)))
      sevenLong must (not be (17L plusOrMinus 2) or not be (17L plusOrMinus 2))

      // Long plusOrMinus Short
      sevenLong must ((not be (19L plusOrMinus 2.toShort)) or (not be (19L plusOrMinus 2.toShort)))
      sevenLong must (not (be (18L plusOrMinus 2.toShort)) or not (be (19L plusOrMinus 2.toShort)))
      sevenLong must (not be (17L plusOrMinus 2.toShort) or not be (17L plusOrMinus 2.toShort))

      // Long plusOrMinus Byte
      sevenLong must ((not be (19L plusOrMinus 2.toByte)) or (not be (19L plusOrMinus 2.toByte)))
      sevenLong must (not (be (18L plusOrMinus 2.toByte)) or not (be (19L plusOrMinus 2.toByte)))
      sevenLong must (not be (17L plusOrMinus 2.toByte) or not be (17L plusOrMinus 2.toByte))

      // Int plusOrMinus Int
      sevenInt must ((not be (19 plusOrMinus 2)) or (not be (19 plusOrMinus 2)))
      sevenInt must (not (be (18 plusOrMinus 2)) or not (be (19 plusOrMinus 2)))
      sevenInt must (not be (17 plusOrMinus 2) or not be (17 plusOrMinus 2))

      // Int plusOrMinus Short
      sevenInt must ((not be (19 plusOrMinus 2.toShort)) or (not be (19 plusOrMinus 2.toShort)))
      sevenInt must (not (be (18 plusOrMinus 2.toShort)) or not (be (19 plusOrMinus 2.toShort)))
      sevenInt must (not be (17 plusOrMinus 2.toShort) or not be (17 plusOrMinus 2.toShort))

      // Int plusOrMinus Byte
      sevenInt must ((not be (19 plusOrMinus 2.toByte)) or (not be (19 plusOrMinus 2.toByte)))
      sevenInt must (not (be (18 plusOrMinus 2.toByte)) or not (be (19 plusOrMinus 2.toByte)))
      sevenInt must (not be (17 plusOrMinus 2.toByte) or not be (17 plusOrMinus 2.toByte))

      // Short plusOrMinus Short
      sevenShort must ((not be (19.toShort plusOrMinus 2.toShort)) or (not be (19.toShort plusOrMinus 2.toShort)))
      sevenShort must (not (be (18.toShort plusOrMinus 2.toShort)) or not (be (19.toShort plusOrMinus 2.toShort)))
      sevenShort must (not be (17.toShort plusOrMinus 2.toShort) or not be (17.toShort plusOrMinus 2.toShort))

      // Short plusOrMinus Byte
      sevenShort must ((not be (19.toShort plusOrMinus 2.toByte)) or (not be (19.toShort plusOrMinus 2.toByte)))
      sevenShort must (not (be (18.toShort plusOrMinus 2.toByte)) or not (be (19.toShort plusOrMinus 2.toByte)))
      sevenShort must (not be (17.toShort plusOrMinus 2.toByte) or not be (17.toShort plusOrMinus 2.toByte))

      // Byte plusOrMinus Byte
      sevenByte must ((not be (19.toByte plusOrMinus 2.toByte)) or (not be (19.toByte plusOrMinus 2.toByte)))
      sevenByte must (not (be (18.toByte plusOrMinus 2.toByte)) or not (be (19.toByte plusOrMinus 2.toByte)))
      sevenByte must (not be (17.toByte plusOrMinus 2.toByte) or not be (17.toByte plusOrMinus 2.toByte))
    }

    it("must throw TestFailedException if the number is not within the specified range") {

      // Double plusOrMinus Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must be (17.1 plusOrMinus 0.2)
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2")

      // Double plusOrMinus Float
      val caught2 = intercept[TestFailedException] {
        sevenDotOh must be (17.1 plusOrMinus 0.2f)
      }
      assert(caught2.getMessage === "7.0 was not 17.1 plus or minus 0.20000000298023224")

      // Double plusOrMinus Long
      val caught3 = intercept[TestFailedException] {
        sevenDotOh must be (17.1 plusOrMinus 2L)
      }
      assert(caught3.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Double plusOrMinus Int
      val caught4 = intercept[TestFailedException] {
        sevenDotOh must be (17.1 plusOrMinus 2)
      }
      assert(caught4.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Double plusOrMinus Short
      val caught5 = intercept[TestFailedException] {
        sevenDotOh must be (17.1 plusOrMinus 2.toShort)
      }
      assert(caught5.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Double plusOrMinus Byte
      val caught6 = intercept[TestFailedException] {
        sevenDotOh must be (17.1 plusOrMinus 2.toByte)
      }
      assert(caught6.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float plusOrMinus Float
      val caught7 = intercept[TestFailedException] {
        sevenDotOhFloat must be (17.1f plusOrMinus 0.2f)
      }
      assert(caught7.getMessage === "7.0 was not 17.1 plus or minus 0.2")

      // Float plusOrMinus Long
      val caught8 = intercept[TestFailedException] {
        sevenDotOhFloat must be (17.1f plusOrMinus 2L)
      }
      assert(caught8.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float plusOrMinus Int
      val caught9 = intercept[TestFailedException] {
        sevenDotOhFloat must be (17.1f plusOrMinus 2)
      }
      assert(caught9.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float plusOrMinus Short
      val caught10 = intercept[TestFailedException] {
        sevenDotOhFloat must be (17.1f plusOrMinus 2.toShort)
      }
      assert(caught10.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Float plusOrMinus Byte
      val caught11 = intercept[TestFailedException] {
        sevenDotOhFloat must be (17.1f plusOrMinus 2.toByte)
      }
      assert(caught11.getMessage === "7.0 was not 17.1 plus or minus 2.0")

      // Long plusOrMinus Long
      val caught12 = intercept[TestFailedException] {
        sevenLong must be (19L plusOrMinus 2L)
      }
      assert(caught12.getMessage === "7 was not 19 plus or minus 2")

      // Long plusOrMinus Int
      val caught13 = intercept[TestFailedException] {
        sevenLong must be (19L plusOrMinus 2)
      }
      assert(caught13.getMessage === "7 was not 19 plus or minus 2")

      // Long plusOrMinus Short
      val caught14 = intercept[TestFailedException] {
        sevenLong must be (19L plusOrMinus 2.toShort)
      }
      assert(caught14.getMessage === "7 was not 19 plus or minus 2")

      // Long plusOrMinus Byte
      val caught15 = intercept[TestFailedException] {
        sevenLong must be (19L plusOrMinus 2.toByte)
      }
      assert(caught15.getMessage === "7 was not 19 plus or minus 2")

      // Int plusOrMinus Int
      val caught16 = intercept[TestFailedException] {
        sevenInt must be (19 plusOrMinus 2)
      }
      assert(caught16.getMessage === "7 was not 19 plus or minus 2")

      // Int plusOrMinus Short
      val caught17 = intercept[TestFailedException] {
        sevenInt must be (19 plusOrMinus 2.toShort)
      }
      assert(caught17.getMessage === "7 was not 19 plus or minus 2")

      // Int plusOrMinus Byte
      val caught18 = intercept[TestFailedException] {
        sevenInt must be (19 plusOrMinus 2.toByte)
      }
      assert(caught18.getMessage === "7 was not 19 plus or minus 2")

      // Short plusOrMinus Short
      val caught19 = intercept[TestFailedException] {
        sevenShort must be (19.toShort plusOrMinus 2.toShort)
      }
      assert(caught19.getMessage === "7 was not 19 plus or minus 2")

      // Short plusOrMinus Byte
      val caught20 = intercept[TestFailedException] {
        sevenShort must be (19.toShort plusOrMinus 2.toByte)
      }
      assert(caught20.getMessage === "7 was not 19 plus or minus 2")

      // Byte plusOrMinus Byte
      val caught21 = intercept[TestFailedException] {
        sevenByte must be (19.toByte plusOrMinus 2.toByte)
      }
      assert(caught21.getMessage === "7 was not 19 plus or minus 2")
    }

    it("must throw TestFailedException if the number is within the specified range, when used with not") {

      // Double plusOrMinus Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must not be (7.1 plusOrMinus 0.2)
      }
      assert(caught1.getMessage === "7.0 was 7.1 plus or minus 0.2")

      // Double plusOrMinus Float
      val caught2 = intercept[TestFailedException] {
        sevenDotOh must not be (7.1 plusOrMinus 0.2f)
      }
      assert(caught2.getMessage === "7.0 was 7.1 plus or minus 0.20000000298023224")

      // Double plusOrMinus Long
      val caught3 = intercept[TestFailedException] {
        sevenDotOh must not be (7.1 plusOrMinus 2L)
      }
      assert(caught3.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Double plusOrMinus Int
      val caught4 = intercept[TestFailedException] {
        sevenDotOh must not be (7.1 plusOrMinus 2)
      }
      assert(caught4.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Double plusOrMinus Short
      val caught5 = intercept[TestFailedException] {
        sevenDotOh must not be (7.1 plusOrMinus 2.toShort)
      }
      assert(caught5.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Double plusOrMinus Byte
      val caught6 = intercept[TestFailedException] {
        sevenDotOh must not be (7.1 plusOrMinus 2.toByte)
      }
      assert(caught6.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float plusOrMinus Float
      val caught7 = intercept[TestFailedException] {
        sevenDotOhFloat must not be (7.1f plusOrMinus 0.2f)
      }
      assert(caught7.getMessage === "7.0 was 7.1 plus or minus 0.2")

      // Float plusOrMinus Long
      val caught8 = intercept[TestFailedException] {
        sevenDotOhFloat must not be (7.1f plusOrMinus 2L)
      }
      assert(caught8.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float plusOrMinus Int
      val caught9 = intercept[TestFailedException] {
        sevenDotOhFloat must not be (7.1f plusOrMinus 2)
      }
      assert(caught9.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float plusOrMinus Short
      val caught10 = intercept[TestFailedException] {
        sevenDotOhFloat must not be (7.1f plusOrMinus 2.toShort)
      }
      assert(caught10.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Float plusOrMinus Byte
      val caught11 = intercept[TestFailedException] {
        sevenDotOhFloat must not be (7.1f plusOrMinus 2.toByte)
      }
      assert(caught11.getMessage === "7.0 was 7.1 plus or minus 2.0")

      // Long plusOrMinus Long
      val caught12 = intercept[TestFailedException] {
        sevenLong must not be (9L plusOrMinus 2L)
      }
      assert(caught12.getMessage === "7 was 9 plus or minus 2")

      // Long plusOrMinus Int
      val caught13 = intercept[TestFailedException] {
        sevenLong must not be (9L plusOrMinus 2)
      }
      assert(caught13.getMessage === "7 was 9 plus or minus 2")

      // Long plusOrMinus Short
      val caught14 = intercept[TestFailedException] {
        sevenLong must not be (9L plusOrMinus 2.toShort)
      }
      assert(caught14.getMessage === "7 was 9 plus or minus 2")

      // Long plusOrMinus Byte
      val caught15 = intercept[TestFailedException] {
        sevenLong must not be (9L plusOrMinus 2.toByte)
      }
      assert(caught15.getMessage === "7 was 9 plus or minus 2")

      // Int plusOrMinus Int
      val caught16 = intercept[TestFailedException] {
        sevenInt must not be (9 plusOrMinus 2)
      }
      assert(caught16.getMessage === "7 was 9 plus or minus 2")

      // Int plusOrMinus Short
      val caught17 = intercept[TestFailedException] {
        sevenInt must not be (9 plusOrMinus 2.toShort)
      }
      assert(caught17.getMessage === "7 was 9 plus or minus 2")

      // Int plusOrMinus Byte
      val caught18 = intercept[TestFailedException] {
        sevenInt must not be (9 plusOrMinus 2.toByte)
      }
      assert(caught18.getMessage === "7 was 9 plus or minus 2")

      // Short plusOrMinus Short
      val caught19 = intercept[TestFailedException] {
        sevenShort must not be (9.toShort plusOrMinus 2.toShort)
      }
      assert(caught19.getMessage === "7 was 9 plus or minus 2")

      // Short plusOrMinus Byte
      val caught20 = intercept[TestFailedException] {
        sevenShort must not be (9.toShort plusOrMinus 2.toByte)
      }
      assert(caught20.getMessage === "7 was 9 plus or minus 2")

      // Byte plusOrMinus Byte
      val caught21 = intercept[TestFailedException] {
        sevenByte must not be (9.toByte plusOrMinus 2.toByte)
      }
      assert(caught21.getMessage === "7 was 9 plus or minus 2")
    }

    it("must throw TestFailedException if the number is not within the specified range, when used in a logical-and expression") {

      // Double plusOrMinus Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must ((be (17.1 plusOrMinus 0.2)) and (be (17.1 plusOrMinus 0.2)))
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh must (be (6.9 plusOrMinus 0.2) and (be (17.1 plusOrMinus 0.2)))
      }
      assert(caught2.getMessage === "7.0 was 6.9 plus or minus 0.2, but 7.0 was not 17.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh must (be (17.0 plusOrMinus 0.2) and be (7.0 plusOrMinus 0.2))
      }
      assert(caught3.getMessage === "7.0 was not 17.0 plus or minus 0.2")

      // Double plusOrMinus Float
      val caught4 = intercept[TestFailedException] {
        sevenDotOh must ((be (17.1 plusOrMinus 0.2f)) and (be (17.1 plusOrMinus 0.2f)))
      }
      assert(caught4.getMessage === "7.0 was not 17.1 plus or minus 0.20000000298023224")
      val caught5 = intercept[TestFailedException] {
        sevenDotOh must (be (6.9 plusOrMinus 0.2f) and (be (17.1 plusOrMinus 0.2f)))
      }
      assert(caught5.getMessage === "7.0 was 6.9 plus or minus 0.20000000298023224, but 7.0 was not 17.1 plus or minus 0.20000000298023224")
      val caught6 = intercept[TestFailedException] {
        sevenDotOh must (be (17.0 plusOrMinus 0.2f) and be (7.0 plusOrMinus 0.2f))
      }
      assert(caught6.getMessage === "7.0 was not 17.0 plus or minus 0.20000000298023224")

      // Double plusOrMinus Long
      val caught7 = intercept[TestFailedException] {
        sevenDotOh must ((be (17.1 plusOrMinus 2L)) and (be (17.1 plusOrMinus 2L)))
      }
      assert(caught7.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught8 = intercept[TestFailedException] {
        sevenDotOh must (be (6.9 plusOrMinus 2L) and (be (17.1 plusOrMinus 2L)))
      }
      assert(caught8.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught9 = intercept[TestFailedException] {
        sevenDotOh must (be (17.0 plusOrMinus 2L) and be (7.0 plusOrMinus 2L))
      }
      assert(caught9.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Double plusOrMinus Int
      val caught10 = intercept[TestFailedException] {
        sevenDotOh must ((be (17.1 plusOrMinus 2)) and (be (17.1 plusOrMinus 2)))
      }
      assert(caught10.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught11 = intercept[TestFailedException] {
        sevenDotOh must (be (6.9 plusOrMinus 2) and (be (17.1 plusOrMinus 2)))
      }
      assert(caught2.getMessage === "7.0 was 6.9 plus or minus 0.2, but 7.0 was not 17.1 plus or minus 0.2")
      val caught12 = intercept[TestFailedException] {
        sevenDotOh must (be (7.0 plusOrMinus 2) and be (17.0 plusOrMinus 2))
      }
      assert(caught12.getMessage === "7.0 was 7.0 plus or minus 2.0, but 7.0 was not 17.0 plus or minus 2.0")

      // Double plusOrMinus Short
      val caught13 = intercept[TestFailedException] {
        sevenDotOh must ((be (17.1 plusOrMinus 2.toShort)) and (be (17.1 plusOrMinus 2.toShort)))
      }
      assert(caught13.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught14 = intercept[TestFailedException] {
        sevenDotOh must (be (6.9 plusOrMinus 2.toShort) and (be (17.1 plusOrMinus 2.toShort)))
      }
      assert(caught14.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught15 = intercept[TestFailedException] {
        sevenDotOh must (be (17.0 plusOrMinus 2.toShort) and be (7.0 plusOrMinus 2.toShort))
      }
      assert(caught15.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Double plusOrMinus Byte
      val caught16 = intercept[TestFailedException] {
        sevenDotOh must ((be (17.1 plusOrMinus 2.toByte)) and (be (17.1 plusOrMinus 2.toByte)))
      }
      assert(caught16.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught17 = intercept[TestFailedException] {
        sevenDotOh must (be (6.9 plusOrMinus 2.toByte) and (be (17.1 plusOrMinus 2.toByte)))
      }
      assert(caught17.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught18 = intercept[TestFailedException] {
        sevenDotOh must (be (17.0 plusOrMinus 2.toByte) and be (7.0 plusOrMinus 2.toByte))
      }
      assert(caught18.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float plusOrMinus Float
      val caught19 = intercept[TestFailedException] {
        sevenDotOhFloat must ((be (17.1f plusOrMinus 0.2f)) and (be (17.1f plusOrMinus 0.2f)))
      }
      assert(caught19.getMessage === "7.0 was not 17.1 plus or minus 0.2")
      val caught20 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (6.9f plusOrMinus 0.2f) and (be (17.1f plusOrMinus 0.2f)))
      }
      assert(caught20.getMessage === "7.0 was 6.9 plus or minus 0.2, but 7.0 was not 17.1 plus or minus 0.2")
      val caught21 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (17.0f plusOrMinus 0.2f) and be (7.0f plusOrMinus 0.2f))
      }
      assert(caught21.getMessage === "7.0 was not 17.0 plus or minus 0.2")

      // Float plusOrMinus Long
      val caught22 = intercept[TestFailedException] {
        sevenDotOhFloat must ((be (17.1f plusOrMinus 2L)) and (be (17.1f plusOrMinus 2L)))
      }
      assert(caught22.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught23 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (6.9f plusOrMinus 2L) and (be (17.1f plusOrMinus 2L)))
      }
      assert(caught23.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught24 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (17.0f plusOrMinus 2L) and be (7.0f plusOrMinus 2L))
      }
      assert(caught24.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float plusOrMinus Int
      val caught25 = intercept[TestFailedException] {
        sevenDotOhFloat must ((be (17.1f plusOrMinus 2)) and (be (17.1f plusOrMinus 2)))
      }
      assert(caught25.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught26 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (6.9f plusOrMinus 2) and (be (17.1f plusOrMinus 2)))
      }
      assert(caught26.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught27 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (17.0f plusOrMinus 2) and be (7.0f plusOrMinus 2))
      }
      assert(caught27.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float plusOrMinus Short
      val caught28 = intercept[TestFailedException] {
        sevenDotOhFloat must ((be (17.1f plusOrMinus 2.toShort)) and (be (17.1f plusOrMinus 2.toShort)))
      }
      assert(caught28.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught29 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (6.9f plusOrMinus 2.toShort) and (be (17.1f plusOrMinus 2.toShort)))
      }
      assert(caught29.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught30 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (17.0f plusOrMinus 2.toShort) and be (7.0f plusOrMinus 2.toShort))
      }
      assert(caught30.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Float plusOrMinus Byte
      val caught31 = intercept[TestFailedException] {
        sevenDotOhFloat must ((be (17.1f plusOrMinus 2.toByte)) and (be (17.1f plusOrMinus 2.toByte)))
      }
      assert(caught31.getMessage === "7.0 was not 17.1 plus or minus 2.0")
      val caught32 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (6.9f plusOrMinus 2.toByte) and (be (17.1f plusOrMinus 2.toByte)))
      }
      assert(caught32.getMessage === "7.0 was 6.9 plus or minus 2.0, but 7.0 was not 17.1 plus or minus 2.0")
      val caught33 = intercept[TestFailedException] {
        sevenDotOhFloat must (be (17.0f plusOrMinus 2.toByte) and be (7.0f plusOrMinus 2.toByte))
      }
      assert(caught33.getMessage === "7.0 was not 17.0 plus or minus 2.0")

      // Long plusOrMinus Long
      val caught34 = intercept[TestFailedException] {
        sevenLong must ((be (19L plusOrMinus 2L)) and (be (9L plusOrMinus 2L)))
      }
      assert(caught34.getMessage === "7 was not 19 plus or minus 2")
      val caught35 = intercept[TestFailedException] {
        sevenLong must (be (18L plusOrMinus 2L) and (be (19L plusOrMinus 2L)))
      }
      assert(caught35.getMessage === "7 was not 18 plus or minus 2")
      val caught36 = intercept[TestFailedException] {
        sevenLong must (be (17L plusOrMinus 2L) and be (7L plusOrMinus 2L))
      }
      assert(caught36.getMessage === "7 was not 17 plus or minus 2")

      // Long plusOrMinus Int
      val caught37 = intercept[TestFailedException] {
        sevenLong must ((be (19L plusOrMinus 2)) and (be (9L plusOrMinus 2)))
      }
      assert(caught37.getMessage === "7 was not 19 plus or minus 2")
      val caught38 = intercept[TestFailedException] {
        sevenLong must (be (8L plusOrMinus 2) and (be (19L plusOrMinus 2)))
      }
      assert(caught38.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught39 = intercept[TestFailedException] {
        sevenLong must (be (17L plusOrMinus 2) and be (7L plusOrMinus 2))
      }
      assert(caught39.getMessage === "7 was not 17 plus or minus 2")

      // Long plusOrMinus Short
      val caught40 = intercept[TestFailedException] {
        sevenLong must ((be (19L plusOrMinus 2.toShort)) and (be (9L plusOrMinus 2.toShort)))
      }
      assert(caught40.getMessage === "7 was not 19 plus or minus 2")
      val caught41 = intercept[TestFailedException] {
        sevenLong must (be (8L plusOrMinus 2.toShort) and (be (19L plusOrMinus 2.toShort)))
      }
      assert(caught41.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught42 = intercept[TestFailedException] {
        sevenLong must (be (17L plusOrMinus 2.toShort) and be (7L plusOrMinus 2.toShort))
      }
      assert(caught42.getMessage === "7 was not 17 plus or minus 2")

      // Long plusOrMinus Byte
      val caught43 = intercept[TestFailedException] {
        sevenLong must ((be (19L plusOrMinus 2.toByte)) and (be (9L plusOrMinus 2.toByte)))
      }
      assert(caught43.getMessage === "7 was not 19 plus or minus 2")
      val caught44 = intercept[TestFailedException] {
        sevenLong must (be (8L plusOrMinus 2.toByte) and (be (19L plusOrMinus 2.toByte)))
      }
      assert(caught44.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught45 = intercept[TestFailedException] {
        sevenLong must (be (17L plusOrMinus 2.toByte) and be (7L plusOrMinus 2.toByte))
      }
      assert(caught45.getMessage === "7 was not 17 plus or minus 2")

      // Int plusOrMinus Int
      val caught46 = intercept[TestFailedException] {
        sevenInt must ((be (19 plusOrMinus 2)) and (be (9 plusOrMinus 2)))
      }
      assert(caught46.getMessage === "7 was not 19 plus or minus 2")
      val caught47 = intercept[TestFailedException] {
        sevenInt must (be (8 plusOrMinus 2) and (be (19 plusOrMinus 2)))
      }
      assert(caught47.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught48 = intercept[TestFailedException] {
        sevenInt must (be (17 plusOrMinus 2) and be (7 plusOrMinus 2))
      }
      assert(caught48.getMessage === "7 was not 17 plus or minus 2")

      // Int plusOrMinus Short
      val caught49 = intercept[TestFailedException] {
        sevenInt must ((be (9 plusOrMinus 2.toShort)) and (be (19 plusOrMinus 2.toShort)))
      }
      assert(caught49.getMessage === "7 was 9 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught50 = intercept[TestFailedException] {
        sevenInt must (be (8 plusOrMinus 2.toShort) and (be (19 plusOrMinus 2.toShort)))
      }
      assert(caught50.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught51 = intercept[TestFailedException] {
        sevenInt must (be (17 plusOrMinus 2.toShort) and be (7 plusOrMinus 2.toShort))
      }
      assert(caught51.getMessage === "7 was not 17 plus or minus 2")

      // Int plusOrMinus Byte
      val caught52 = intercept[TestFailedException] {
        sevenInt must ((be (19 plusOrMinus 2.toByte)) and (be (9 plusOrMinus 2.toByte)))
      }
      assert(caught52.getMessage === "7 was not 19 plus or minus 2")
      val caught53 = intercept[TestFailedException] {
        sevenInt must (be (8 plusOrMinus 2.toByte) and (be (19 plusOrMinus 2.toByte)))
      }
      assert(caught53.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught54 = intercept[TestFailedException] {
        sevenInt must (be (17 plusOrMinus 2.toByte) and be (7 plusOrMinus 2.toByte))
      }
      assert(caught54.getMessage === "7 was not 17 plus or minus 2")

      // Short plusOrMinus Short
      val caught55 = intercept[TestFailedException] {
        sevenShort must ((be (19.toShort plusOrMinus 2.toShort)) and (be (9.toShort plusOrMinus 2.toShort)))
      }
      assert(caught55.getMessage === "7 was not 19 plus or minus 2")
      val caught56 = intercept[TestFailedException] {
        sevenShort must (be (8.toShort plusOrMinus 2.toShort) and (be (19.toShort plusOrMinus 2.toShort)))
      }
      assert(caught56.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught57 = intercept[TestFailedException] {
        sevenShort must (be (17.toShort plusOrMinus 2.toShort) and be (7.toShort plusOrMinus 2.toShort))
      }
      assert(caught57.getMessage === "7 was not 17 plus or minus 2")

      // Short plusOrMinus Byte
      val caught58 = intercept[TestFailedException] {
        sevenShort must ((be (19.toShort plusOrMinus 2.toByte)) and (be (9.toShort plusOrMinus 2.toByte)))
      }
      assert(caught58.getMessage === "7 was not 19 plus or minus 2")
      val caught59 = intercept[TestFailedException] {
        sevenShort must (be (8.toShort plusOrMinus 2.toByte) and (be (19.toShort plusOrMinus 2.toByte)))
      }
      assert(caught59.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught60 = intercept[TestFailedException] {
        sevenShort must (be (17.toShort plusOrMinus 2.toByte) and be (7.toShort plusOrMinus 2.toByte))
      }
      assert(caught60.getMessage === "7 was not 17 plus or minus 2")

      // Byte plusOrMinus Byte
      val caught61 = intercept[TestFailedException] {
        sevenByte must ((be (19.toByte plusOrMinus 2.toByte)) and (be (9.toByte plusOrMinus 2.toByte)))
      }
      assert(caught61.getMessage === "7 was not 19 plus or minus 2")
      val caught62 = intercept[TestFailedException] {
        sevenByte must (be (8.toByte plusOrMinus 2.toByte) and (be (19.toByte plusOrMinus 2.toByte)))
      }
      assert(caught62.getMessage === "7 was 8 plus or minus 2, but 7 was not 19 plus or minus 2")
      val caught63 = intercept[TestFailedException] {
        sevenByte must (be (17.toByte plusOrMinus 2.toByte) and be (7.toByte plusOrMinus 2.toByte))
      }
      assert(caught63.getMessage === "7 was not 17 plus or minus 2")
    }

    it("must throw TestFailedException if the number is not within the specified range, when used in a logical-or expression") {

      // Double plusOrMinus Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must ((be (17.1 plusOrMinus 0.2)) or (be (17.1 plusOrMinus 0.2)))
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2, and 7.0 was not 17.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh must (be (16.9 plusOrMinus 0.2) or (be (17.1 plusOrMinus 0.2)))
      }
      assert(caught2.getMessage === "7.0 was not 16.9 plus or minus 0.2, and 7.0 was not 17.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh must (be (17.0 plusOrMinus 0.2) or be (97.0 plusOrMinus 0.2))
      }
      assert(caught3.getMessage === "7.0 was not 17.0 plus or minus 0.2, and 7.0 was not 97.0 plus or minus 0.2")
    }

    it("must throw TestFailedException if the number is within the specified range, when used in a logical-and expression with not") {

      // Double plusOrMinus Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must (not (be (17.1 plusOrMinus 0.2)) and not (be (7.1 plusOrMinus 0.2)))
      }
      assert(caught1.getMessage === "7.0 was not 17.1 plus or minus 0.2, but 7.0 was 7.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh must (not be (16.9 plusOrMinus 0.2) and (not be (7.1 plusOrMinus 0.2)))
      }
      assert(caught2.getMessage === "7.0 was not 16.9 plus or minus 0.2, but 7.0 was 7.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh must (not be (17.0 plusOrMinus 0.2) and not be (7.0 plusOrMinus 0.2))
      }
      assert(caught3.getMessage === "7.0 was not 17.0 plus or minus 0.2, but 7.0 was 7.0 plus or minus 0.2")

      // Check that the error message "short circuits"
      val caught4 = intercept[TestFailedException] {
        sevenDotOh must (not (be (7.1 plusOrMinus 0.2)) and not (be (7.1 plusOrMinus 0.2)))
      }
      assert(caught4.getMessage === "7.0 was 7.1 plus or minus 0.2")
    }

    it("must throw TestFailedException if the number is within the specified range, when used in a logical-or expression with not") {

      // Double plusOrMinus Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must (not (be (7.1 plusOrMinus 0.2)) or not (be (7.1 plusOrMinus 0.2)))
      }
      assert(caught1.getMessage === "7.0 was 7.1 plus or minus 0.2, and 7.0 was 7.1 plus or minus 0.2")
      val caught2 = intercept[TestFailedException] {
        sevenDotOh must ((not be (6.9 plusOrMinus 0.2)) or (not be (7.1 plusOrMinus 0.2)))
      }
      assert(caught2.getMessage === "7.0 was 6.9 plus or minus 0.2, and 7.0 was 7.1 plus or minus 0.2")
      val caught3 = intercept[TestFailedException] {
        sevenDotOh must (not be (7.0 plusOrMinus 0.2) or not be (7.0 plusOrMinus 0.2))
      }
      assert(caught3.getMessage === "7.0 was 7.0 plus or minus 0.2, and 7.0 was 7.0 plus or minus 0.2")
    }

    it("must throw TestFailedException if the number passed as the range is 0 or negative") {

      // Double plusOrMinus Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh must be (7.1 plusOrMinus -0.2)
      }
      assert(caught1.getMessage === "Range (-0.2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Double plusOrMinus Float
      val caught2 = intercept[TestFailedException] {
        sevenDotOh must be (7.1 plusOrMinus -0.2f)
      }
      assert(caught2.getMessage === "Range (-0.20000000298023224) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Double plusOrMinus Long
      val caught3 = intercept[TestFailedException] {
        sevenDotOh must be (7.1 plusOrMinus -2L)
      }
      assert(caught3.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Double plusOrMinus Int
      val caught4 = intercept[TestFailedException] {
        sevenDotOh must be (7.1 plusOrMinus -2)
      }
      assert(caught4.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Double plusOrMinus Short
      val caught5 = intercept[TestFailedException] {
        sevenDotOh must be (7.1 plusOrMinus (-2).toShort)
      }
      assert(caught5.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Double plusOrMinus Byte
      val caught6 = intercept[TestFailedException] {
        sevenDotOh must be (7.1 plusOrMinus (-2).toByte)
      }
      assert(caught6.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Float plusOrMinus Float
      val caught7 = intercept[TestFailedException] {
        sevenDotOhFloat must be (7.1f plusOrMinus -0.2f)
      }
      assert(caught7.getMessage === "Range (-0.2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Float plusOrMinus Long
      val caught8 = intercept[TestFailedException] {
        sevenDotOhFloat must be (7.1f plusOrMinus -2L)
      }
      assert(caught8.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Float plusOrMinus Int
      val caught9 = intercept[TestFailedException] {
        sevenDotOhFloat must be (7.1f plusOrMinus -2)
      }
      assert(caught9.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Float plusOrMinus Short
      val caught10 = intercept[TestFailedException] {
        sevenDotOhFloat must be (7.1f plusOrMinus (-2).toShort)
      }
      assert(caught10.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Float plusOrMinus Byte
      val caught11 = intercept[TestFailedException] {
        sevenDotOhFloat must be (7.1f plusOrMinus (-2).toByte)
      }
      assert(caught11.getMessage === "Range (-2.0) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Long plusOrMinus Long
      val caught12 = intercept[TestFailedException] {
        sevenLong must be (9L plusOrMinus -2L)
      }
      assert(caught12.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Long plusOrMinus Int
      val caught13 = intercept[TestFailedException] {
        sevenLong must be (9L plusOrMinus -2)
      }
      assert(caught13.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Long plusOrMinus Short
      val caught14 = intercept[TestFailedException] {
        sevenLong must be (9L plusOrMinus (-2).toShort)
      }
      assert(caught14.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Long plusOrMinus Byte
      val caught15 = intercept[TestFailedException] {
        sevenLong must be (9L plusOrMinus (-2).toByte)
      }
      assert(caught15.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Int plusOrMinus Int
      val caught16 = intercept[TestFailedException] {
        sevenInt must be (9 plusOrMinus -2)
      }
      assert(caught16.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Int plusOrMinus Short
      val caught17 = intercept[TestFailedException] {
        sevenInt must be (9 plusOrMinus (-2).toShort)
      }
      assert(caught17.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Int plusOrMinus Byte
      val caught18 = intercept[TestFailedException] {
        sevenInt must be (9 plusOrMinus (-2).toByte)
      }
      assert(caught18.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Short plusOrMinus Short
      val caught19 = intercept[TestFailedException] {
        sevenShort must be (9.toShort plusOrMinus (-2).toShort)
      }
      assert(caught19.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Short plusOrMinus Byte
      val caught20 = intercept[TestFailedException] {
        sevenShort must be (9.toShort plusOrMinus (-2).toByte)
      }
      assert(caught20.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")

      // Byte plusOrMinus Byte
      val caught21 = intercept[TestFailedException] {
        sevenByte must be (9.toByte plusOrMinus (-2).toByte)
      }
      assert(caught21.getMessage === "Range (-2) passed to plusOrMinus was zero or negative. Must be a positive non-zero number.")
    }
  }
}
