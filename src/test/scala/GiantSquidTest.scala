import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class GiantSquidTest extends AnyFlatSpec {
  "The score of the winner board " should "be 4512 " in {
    assert(GiantSquid.winnerScore(Source.fromResource("GiantSquid1").getLines().toList) == 4512)
  }

  "The score of the winner board " should "be 21607 in Exercise " in {
    assert(GiantSquid.winnerScore(Source.fromResource("GiantSquid2").getLines().toList) == 21607)
  }

  "The score for the last board to win" should "be 21607 " in {
    assert(GiantSquid.winnerScoreLast(Source.fromResource("GiantSquid1").getLines().toList) == 1924)
  }

  "The score for the last board to win" should "be 19012 in exercise" in {
    assert(GiantSquid.winnerScoreLast(Source.fromResource("GiantSquid2").getLines().toList) == 19012)
  }
}
