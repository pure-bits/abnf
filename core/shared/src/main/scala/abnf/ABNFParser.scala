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

import cats.parse._
import cats.parse.{Parser => P}
import cats.parse.Rfc5234._

import cats.data._
import cats.syntax.all._

object ABNFParser {
  import ABNF._
  import ABNF.Expr._

  private val hyphen: Parser[Unit] = P.char('-')

  val numValue: Parser[Either[Numeric, Either[NumericRange, NumericConcatenation]]] = {
    val percent: Parser[Unit] = P.char('%')
    val base: Parser[Numeric.Base] =
      P.char('x').as(Numeric.Base.Hex) |
        P.char('d').as(Numeric.Base.Dec) |
        P.char('b').as(Numeric.Base.Bin)

    (percent *> base).flatMap { base =>
      val (radix, parser) = base match {
        case Numeric.Base.Hex => (16, hexdig)
        case Numeric.Base.Dec => (10, digit)
        case Numeric.Base.Bin => (2, bit)
      }

      val intParser: Parser[Int] = parser.rep.string.map(Integer.parseInt(_, radix))

      (
        intParser ~ (P.char('.') *> intParser).rep.eitherOr(hyphen *> intParser).?
      ).map {
        case (first, None) => Left(Numeric(first, base))
        case (first, Some(Left(second))) => Right(Left(NumericRange(first, second, base)))
        case (first, Some(Right(concat))) => Right(Right(NumericConcatenation(first :: concat, base)))
      }
    }.withContext("num-value")
  }

  val charValue: Parser[Text] = {
    // string of SP and VCHAR without DQUOTE
    val text: Parser0[String] = (P.charIn(0x20, 0x21) | P.charIn(0x23.toChar to 0x7e.toChar)).rep0.string
    text.with1.surroundedBy(dquote).map(Text).withContext("char-value")
  }

  val crlf: Parser[Unit] = Rfc5234.crlf.backtrack | Rfc5234.lf

  // ";" *(WSP / VCHAR) CRLF
  val comment: Parser[Unit] = (P.char(';') ~ (wsp | vchar).rep0 ~ crlf).void.withContext("comment")

  // comment / CRLF
  val cnl: Parser[Unit] = (comment | crlf).withContext("cnl")

  //  WSP / (c-nl WSP)
  val cwsp: Parser[Unit] = (wsp | (cnl ~ wsp)).void.withContext("cwsp")

  val cwsp0: Parser0[Unit] = cwsp.rep0.void.withContext("cwsp0")

  // ALPHA *(ALPHA / DIGIT / "-")
  val ruleName: Parser[NonTerminal] = {
    (alpha ~ (alpha | digit | hyphen).rep0).string
      .map(NonTerminal)
      .withContext("rulename")
  }

  val alternation: Parser[ABNF.Expr] = P.recursive[ABNF.Expr] { expr =>
    // "[" *c-wsp alternation *c-wsp "]"
    val option: Parser[Expr.Optional] = {
      expr
        .between(P.char('[') ~ cwsp0, cwsp0 ~ P.char(']'))
        .map(Optional)
        .withContext("option")
    }

    // "(" *c-wsp alternation *c-wsp ")"
    val group: Parser[Expr.Group] = {
      expr
        .between(P.char('(') ~ cwsp0, cwsp0 ~ P.char(')'))
        .map(Group)
        .withContext("group")
    }

    // rulename / group / option / char-val / num-val / prose-val
    val element: Parser[ABNF.Expr] = {
      ruleName |
        group |
        option |
        charValue |
        numValue.map(_.joinRight[Expr, Either[Expr, Expr], Expr].merge)
    }.withContext("element")

    // [repeat] element
    // repeat = 1*DIGIT / (*DIGIT "*" *DIGIT)
    val repetition: Parser[Expr] = {
      val maybeNumber: Parser0[Option[Int]] =
        digit.rep.string.?.map(_.map(Integer.parseInt(_, 10)))

      val repeat: Parser0[Either[Int, (Option[Int], Option[Int])]] =
        ((maybeNumber <* P.char('*')) ~ maybeNumber).backtrack
          .eitherOr(digit.rep.string.map(Integer.parseInt(_, 10)))
          .withContext("repeat")

      (repeat.?.with1 ~ element).map {
        case (Some(repeat), x) => Expr.Repetition(repeat, x)
        case (None, x) => x
      }.withContext("repetition")
    }

    // repetition *(1*c-wsp repetition)
    val concatenation: Parser[ABNF.Expr] = {
      (repetition ~ (cwsp.rep *> repetition).backtrack.rep0).map {
        case (h, t) => t.foldLeft(h)(Concatenation)
      }.withContext("concatenation")
    }

    // concatenation *(*c-wsp "/" *c-wsp concatenation)
    (concatenation ~ (P.char('/').surroundedBy(cwsp0) *> concatenation).backtrack.rep0).map {
      case (h, t) => t.foldLeft(h)(Alternation)
    }.withContext("alternation")
  }

  val elements: Parser[ABNF.Expr] = {
    (alternation <* (wsp | (cnl ~ wsp)).backtrack.rep0).withContext("elements")
  }

  // rulename defined-as elements c-nl
  val rule: Parser[Rule] = {
    // *c-wsp ("=" / "=/") *c-wsp
    val definedAs: Parser[Rule.Kind] = {
      val ruleKind: Parser[Rule.Kind] =
        P.char('=').as(Rule.Kind.Basic) |
          P.string("=/").as(Rule.Kind.IncrementalAlternatives)

      ruleKind.surroundedBy(cwsp0).withContext("definedAs")
    }

    (ruleName ~ definedAs ~ elements <* cnl).map {
      case ((name, kind), definition) =>
        ABNF.Rule(name, kind, definition)
    }.withContext("rule")
  }

  // 1*( rule / (*c-wsp c-nl) )
  val ruleList: Parser[List[Rule]] = {
    rule
      .eitherOr((wsp | cnl).void.withContext("cwsp"))
      .rep
      .map(_.collect { case Right(rule) => rule })
      .withContext("rulelist")
  }

  val grammar: Parser[ABNF.Grammar] =
    ruleList
      .mapFilter(NonEmptyList.fromList)
      .map(ABNF.Grammar)
      .withContext("grammar")
}
