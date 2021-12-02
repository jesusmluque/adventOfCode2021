import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.io.Source

class SonarSweepTest extends AnyFlatSpec {
  "The number of times a deph increases in the example provided " should "be 7" in {
    assert(SonarSweep.timesOfIncreases(List(199,200,208,210,200,207,240,269,260,263)) == 7)
  }

  "The number of times a deph increases in the example 1 " should "be 8" in {
    assert(SonarSweep.timesOfIncreases(List(199,200,208,210,200,199,188,207,207,208,240,269,260,263)) == 8)
  }

  "The number of times a deph increases in the large exercise " should "be 1215" in {
    val input = Source.fromResource("SonarSweep1").getLines.toList.map(_.toInt)

    assert(SonarSweep.timesOfIncreases(input) == 1215)
  }

  "The number of times a deph increases each 3-window in the example provided " should "be 5" in {
    assert(SonarSweep.timesOfIncreasesWithWindow(List(199,200,208,210,200,207,240,269,260,263)) == 5)
  }

  "The number of times a deph increases each 3-window in the large exercise " should "be 1150" in {
    val input = Source.fromResource("SonarSweep1").getLines.toList.map(_.toInt)

    assert(SonarSweep.timesOfIncreasesWithWindow(input) == 1150)
  }

}
