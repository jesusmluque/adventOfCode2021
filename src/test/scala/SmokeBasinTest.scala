import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class SmokeBasinTest extends AnyFlatSpec {

  "The sum of the risk level for the example " should " be 15 " in {
    assert(SmokeBasin.findRiskLevelFor(Source.fromResource("smokeBasin1").getLines().toList) == 15)
  }

  "The sum of the risk level for the exercise " should " be 508 " in {
    assert(SmokeBasin.findRiskLevelFor(Source.fromResource("smokeBasin2").getLines().toList) == 508)
  }

  "The product of the size of the three largest basins on the example " should " be 1134 " in {
    assert(SmokeBasin.findProductOfThreeLargestBasinsFor(Source.fromResource("smokeBasin1").getLines().toList) == 1134)
  }

  "The product of the size of the three largest basins on the exercise " should " be 1564640 " in {
    assert(SmokeBasin.findProductOfThreeLargestBasinsFor(Source.fromResource("smokeBasin2").getLines().toList) == 1564640)
  }
}
