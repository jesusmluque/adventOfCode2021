import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class SevenSegmentTest extends AnyFlatSpec {

  "The digits 1, 4, 7 and 8 " should " appear 26 in the output of the example " in {
    assert(SevenSegment.countOnlyOutputDigitsForUniqueNumberOfSegment(Source.fromResource("SevenSegment2").getLines().toList) == 26)
  }

  "The digits 1, 4, 7 and 8 " should " appear 456 in the output of the exercise " in {
    assert(SevenSegment.countOnlyOutputDigitsForUniqueNumberOfSegment(Source.fromResource("SevenSegment1").getLines().toList) == 456)
  }

  "Adding all the output numbers " should "be 61229 in the example" in {
    assert(SevenSegment.addAllOutputNumbers(Source.fromResource("SevenSegment2").getLines().toList) == 61229)
  }

  "Adding all the output numbers " should "be 1091609 in the exercise" in {
    assert(SevenSegment.addAllOutputNumbers(Source.fromResource("SevenSegment1").getLines().toList) == 1091609)
  }
}
