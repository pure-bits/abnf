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

import abnf.ABNF
import abnf.ABNF.Expr
import abnf.algebra.ABNFExprF._
import cats.Functor
import higherkindness.droste.{Algebra, Basis, Coalgebra}

package object droste {

  implicit val functor: Functor[ABNFExprF] = {
    new Functor[ABNFExprF] {
      override def map[A, B](fa: ABNFExprF[A])(f: A => B): ABNFExprF[B] = {
        fa match {
          case v @ (
                _: NonTerminalF[_] | _: TextF[_] | _: NumericF[_] | _: NumericRangeF[_] | _: NumericConcatenationF[_]
              ) =>
            v.asInstanceOf[ABNFExprF[B]]

          case AlternationF(left, right) => AlternationF(f(left), f(right))
          case ConcatenationF(left, right) => ConcatenationF(f(left), f(right))
          case RepetitionF(repetition, value) => RepetitionF(repetition, f(value))
          case GroupF(value) => GroupF(f(value))
          case OptionalF(value) => OptionalF(f(value))
        }
      }
    }
  }

  implicit val toABNFExpr: Algebra[ABNFExprF, ABNF.Expr] = Algebra {
    case NonTerminalF(value) => ABNF.Expr.NonTerminal(value)
    case TextF(value) => ABNF.Expr.Text(value)
    case NumericF(value, base) => ABNF.Expr.Numeric(value, base)
    case NumericRangeF(from, to, base) => ABNF.Expr.NumericRange(from, to, base)
    case NumericConcatenationF(values, base) => ABNF.Expr.NumericConcatenation(values, base)
    case AlternationF(left, right) => ABNF.Expr.Alternation(left, right)
    case ConcatenationF(left, right) => ABNF.Expr.Concatenation(left, right)
    case RepetitionF(repetition, value) => ABNF.Expr.Repetition(repetition, value)
    case GroupF(value) => ABNF.Expr.Group(value)
    case OptionalF(value) => ABNF.Expr.Optional(value)
  }

  implicit val fromABNFExpr: Coalgebra[ABNFExprF, ABNF.Expr] = Coalgebra {
    case Expr.NonTerminal(value) => ABNFExprF.NonTerminalF(value)
    case Expr.Text(value) => ABNFExprF.TextF(value)
    case Expr.Numeric(value, base) => ABNFExprF.NumericF(value, base)
    case Expr.NumericRange(from, to, base) => ABNFExprF.NumericRangeF(from, to, base)
    case Expr.NumericConcatenation(values, base) => ABNFExprF.NumericConcatenationF(values, base)
    case Expr.Alternation(left, right) => ABNFExprF.AlternationF(left, right)
    case Expr.Concatenation(left, right) => ABNFExprF.ConcatenationF(left, right)
    case Expr.Repetition(repetition, value) => ABNFExprF.RepetitionF(repetition, value)
    case Expr.Group(value) => ABNFExprF.GroupF(value)
    case Expr.Optional(value) => ABNFExprF.OptionalF(value)
  }

  implicit val basis: Basis[ABNFExprF, ABNF.Expr] =
    Basis.Default(toABNFExpr, fromABNFExpr)
}
