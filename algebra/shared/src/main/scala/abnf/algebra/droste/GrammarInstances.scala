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
import abnf.ABNF.{Grammar, Rule}
import abnf.algebra.format
import cats.Show
import higherkindness.droste.scheme

trait GrammarInstances {

  implicit val showGrammar: Show[Grammar] = { grammar =>
    val definition: ABNF.Expr => String = scheme.cata(format)

    val definedAs: Rule.Kind => String = {
      case Rule.Kind.Basic => "="
      case Rule.Kind.IncrementalAlternatives => "=/"
    }

    grammar.rules.iterator
      .map(r => s"${r.name.value} ${definedAs(r.kind)} ${definition(r.definition)}")
      .mkString(";\n")
  }
}
