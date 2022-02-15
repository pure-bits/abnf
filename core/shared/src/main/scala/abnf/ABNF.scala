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
import cats.data.NonEmptyList

object ABNF {

  sealed trait Expr

  object Expr {

    case class NonTerminal(value: String) extends Expr

    case class Text(value: String) extends Expr

    case class Numeric(value: Int, base: Base) extends Expr

    object Numeric {
      sealed trait Base
      object Base {
        case object Hex extends Base
        case object Dec extends Base
        case object Bin extends Base
      }
    }

    case class NumericRange(from: Int, to: Int, base: Base) extends Expr

    case class NumericConcatenation(values: NonEmptyList[Int], base: Base) extends Expr

    case class Alternation(left: Expr, right: Expr) extends Expr

    case class Concatenation(left: Expr, right: Expr) extends Expr

    case class Repetition(
      repetition: Either[Int, (Option[Int], Option[Int])],
      value: Expr
    ) extends Expr

    case class Group(value: Expr) extends Expr

    case class Optional(value: Expr) extends Expr
  }

  case class Rule(
    name: Expr.NonTerminal,
    kind: Rule.Kind,
    definition: Expr
  )

  object Rule {
    sealed trait Kind
    object Kind {
      case object Basic extends Kind
      case object IncrementalAlternatives extends Kind
    }
  }

  case class Grammar(rules: NonEmptyList[Rule])
}
