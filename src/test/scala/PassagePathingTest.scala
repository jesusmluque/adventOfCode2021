import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class PassagePathingTest extends AnyFlatSpec {

  "The number of paths through the cave of the example1 that visit the small cave at most one " should " be 10 " in {
    assert(PassagePathing.calculateNumberOfPaths(Source.fromResource("PassagePathing1").getLines().toList) == 10)
  }

  "The number of paths through the cave of the example2 that visit the small cave at most one " should " be 19 " in {
    assert(PassagePathing.calculateNumberOfPaths(Source.fromResource("PassagePathing2").getLines().toList) == 19)
  }

  "The number of paths through the cave of the example3 that visit the small cave at most one " should " be 226 " in {
    assert(PassagePathing.calculateNumberOfPaths(Source.fromResource("PassagePathing3").getLines().toList) == 226)
  }

  "The number of paths through the cave of the exercise that visit the small cave at most one " should " be 5157 " in {
    assert(PassagePathing.calculateNumberOfPaths(Source.fromResource("PassagePathing4").getLines().toList) == 5157)
  }

  "The number of paths through the cave of the example1 that visit the small cave at most one except one single cave that can be visited twice " should " be 36 " in {
    assert(PassagePathing.calculateNumberOfPathsWithSingleTwice(Source.fromResource("PassagePathing1").getLines().toList) == 36)
  }

  "The number of paths through the cave of the example3 that visit the small cave at most one except one single cave that can be visited twice " should " be 103 " in {
    assert(PassagePathing.calculateNumberOfPathsWithSingleTwice(Source.fromResource("PassagePathing2").getLines().toList) == 103)
  }

  "The number of paths through the cave of the exercise that visit the small cave at most one except one single cave that can be visited twice " should " be 144309 " in {
    assert(PassagePathing.calculateNumberOfPathsWithSingleTwice(Source.fromResource("PassagePathing4").getLines().toList) == 144309)
  }
}
