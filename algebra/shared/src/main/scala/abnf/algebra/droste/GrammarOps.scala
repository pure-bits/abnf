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

package abnf.algebra.droste

import abnf.ABNF
import abnf.ABNF.Grammar
import abnf.algebra._
import higherkindness.droste._

trait GrammarSyntax {

  implicit def toGrammarOps(grammar: Grammar): GrammarOps =
    new GrammarOps(grammar)
}

final class GrammarOps(private val grammar: Grammar) extends AnyVal {

  def isComplete: Boolean = {
    val getNonTerminals: ABNF.Expr => Set[String] = scheme.cata(nonTerminals)

    val (leftNonTerminals, rightNonTerminals) = grammar.rules.foldLeft(
      (
        Set.newBuilder[String],
        Set.newBuilder[String]
      )) {
      case ((leftNT, rightNT), rule) =>
        (
          leftNT.addOne(rule.name.value),
          rightNT.addAll(getNonTerminals(rule.definition))
        )
    }

    rightNonTerminals.result().subsetOf(leftNonTerminals.result())
  }
}
