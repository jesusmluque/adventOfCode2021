import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class LanternfishTest extends AnyFlatSpec {

  "the number of lanthern fish after 80 days when the initial are 3,4,3,1,2 " should "be 5934 " in {
    assert(Lanternfish.calculate(List(3,4,3,1,2), 80) == 5934)
  }

  "the number of lanthern fish after 80 days when the initial come from the Exercise " should "be 352872 " in {
    assert(Lanternfish.calculate(Source.fromResource("Lanternfish1").getLines().next().split(",").map(_.toInt).toList, 80) == 352872)
  }

  "the number of lanthern fish after 256 days when the initial come from the Exercise " should "be 1604361182149 " in {
    assert(Lanternfish.calculate(Source.fromResource("Lanternfish1").getLines().next().split(",").map(_.toInt).toList, 256) == 1604361182149L)
  }
}
