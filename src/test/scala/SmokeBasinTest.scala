import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class SmokeBasinTest extends AnyFlatSpec {

  "The sum of the risk level for the example " should " be 15 " in {
    assert(SmokeBasin.findRiskLevelFor(Source.fromResource("smokeBasin1").getLines().toList) == 15)
  }
}
