import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class SyntaxScoringTest extends AnyFlatSpec {

  "The total syntax error score for the example instructions set " should " be 26397 " in {
    assert(SyntaxScore.getErrorScoreIn(Source.fromResource("SyntaxScoring1").getLines().toList) == 26397L)
  }

  "The total syntax error score for the exercise instructions set " should " be 362271 " in {
    assert(SyntaxScore.getErrorScoreIn(Source.fromResource("SyntaxScoring2").getLines().toList) == 362271L)
  }

  "The middle score for the exemple set " should " be 288957 " in {
    assert(SyntaxScore.getValidInstructionsScore(Source.fromResource("SyntaxScoring1").getLines().toList) == 288957)
  }

  "The middle score for the exercise set " should " be 1698395182 " in {
    assert(SyntaxScore.getValidInstructionsScore(Source.fromResource("SyntaxScoring2").getLines().toList) == 1698395182L)
  }
}
