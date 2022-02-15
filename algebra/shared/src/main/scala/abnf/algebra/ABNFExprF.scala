/*
 * Copyright (c) 2022 the abnf contributors.
 * See the project homepage at: https://purebits.github.io/abnf/
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

package abnf.algebra

import abnf.ABNF.Expr.Numeric.Base
import cats.data.NonEmptyList

sealed trait ABNFExprF[A]

object ABNFExprF {

  case class NonTerminalF[A](value: String) extends ABNFExprF[A]
  case class TextF[A](value: String) extends ABNFExprF[A]
  case class NumericF[A](value: Int, base: Base) extends ABNFExprF[A]
  case class NumericRangeF[A](from: Int, to: Int, base: Base) extends ABNFExprF[A]
  case class NumericConcatenationF[A](values: NonEmptyList[Int], base: Base) extends ABNFExprF[A]
  case class AlternationF[A](left: A, right: A) extends ABNFExprF[A]
  case class ConcatenationF[A](left: A, right: A) extends ABNFExprF[A]

  case class RepetitionF[A](
    repetition: Either[Int, (Option[Int], Option[Int])],
    value: A
  ) extends ABNFExprF[A]

  case class GroupF[A](value: A) extends ABNFExprF[A]
  case class OptionalF[A](value: A) extends ABNFExprF[A]

}
