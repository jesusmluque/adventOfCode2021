import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class SyntaxScoringTest extends AnyFlatSpec {

  "The total syntax error score for the example instructions set " should " be 26397 " in {
    assert(SyntaxScore.getErrorScoreIn(Source.fromResource("SyntaxScoring1").getLines().toList) == 26397L)
  }
}
