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

package abnf

import abnf.ABNF.Expr.Numeric.Base
import abnf.algebra.ABNFExprF._
import higherkindness.droste._

package object algebra {

  val format: Algebra[ABNFExprF, String] = {
    val baseFormat: Base => String = {
      case Base.Hex => "%x"
      case Base.Dec => "%d"
      case Base.Bin => "%b"
    }

    def intFormat(value: Int, b: Base): String = {
      val radix = b match {
        case Base.Hex => 16
        case Base.Dec => 10
        case Base.Bin => 2
      }
      Integer.toString(value, radix)
    }

    Algebra {
      case NonTerminalF(value) => value

      case TextF(value) => "\"" + value + "\""

      case NumericF(value, base) =>
        baseFormat(base) + intFormat(value, base)

      case NumericRangeF(from, to, base) =>
        baseFormat(base) + intFormat(from, base) + "-" + intFormat(to, base)

      case NumericConcatenationF(values, base) =>
        baseFormat(base) + values.iterator.map(intFormat(_, base)).mkString(".")

      case AlternationF(left, right) => left + " | " + right

      case ConcatenationF(left, right) => left + " " + right

      case RepetitionF(repetition, value) =>
        val rep = repetition match {
          case Left(exact) => exact.toString
          case Right((from, to)) =>
            from.fold("")(_.toString) + "*" + to.fold("")(_.toString)
        }

        rep + value

      case GroupF(value) => "(" + value + ")"

      case OptionalF(value) => "[" + value + "]"
    }
  }

  val nonTerminals: Algebra[ABNFExprF, Set[String]] = Algebra {
    case NonTerminalF(value) => Set(value)
    case AlternationF(left, right) => left ++ right
    case ConcatenationF(left, right) => left ++ right
    case RepetitionF(_, value) => value
    case GroupF(value) => value
    case OptionalF(value) => value
    case _ => Set.empty
  }

  val terminals: Algebra[ABNFExprF, Set[String]] = Algebra {
    case TextF(value) => Set(value)
    case n @ (_: NumericF[_] | _: NumericRangeF[_] | _: NumericConcatenationF[_]) =>
      Set(format.apply(n.asInstanceOf[ABNFExprF[String]]))

    case AlternationF(left, right) => left ++ right
    case ConcatenationF(left, right) => left ++ right
    case RepetitionF(_, value) => value
    case GroupF(value) => value
    case OptionalF(value) => value
    case _ => Set.empty
  }

}
