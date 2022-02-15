package abnf.algebra.droste

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.util.Using

class GrammarOpsSpec extends AnyFunSuite {

  test("parse ABNF grammar check whether it is valid and format it") {

    val abnfDefinition =
      Using.resource(Source.fromResource("abnf.abnf"))(
        _.getLines().mkString(start = "", sep = "\n", end = "\n")
      )

    assert(
      abnf.ABNFParser.grammar.parseAll(abnfDefinition).isRight,
      "failed to parse grammar"
    )
  }
}
