import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class DumboOctopusTest extends AnyFlatSpec{

  "After 100 steps, the number of flashes of dumb octopus in the exemple grid " should " b 1656 " in {
    assert(DumboOctopus.flashesAfter(100, Source.fromResource("DumboOctopus1").getLines().toList) == 1656)
  }

  "After 100 steps, the number of flashes of dumb octopus in the exercise grid " should " b 1683 " in {
    assert(DumboOctopus.flashesAfter(100, Source.fromResource("DumboOctopus2").getLines().toList) == 1683)
  }

  "The step where all octupus flash at the same time for the example " should " be 195 " in {
    assert(DumboOctopus.allFlashesAtSameTime(Source.fromResource("DumboOctopus1").getLines().toList) == 195)
  }

  "The step where all octupus flash at the same time for the exercise " should " be 788 " in {
    assert(DumboOctopus.allFlashesAtSameTime(Source.fromResource("DumboOctopus2").getLines().toList) == 788)
  }
}
