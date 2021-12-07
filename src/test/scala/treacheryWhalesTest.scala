import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class treacheryWhalesTest extends AnyFlatSpec {

  "The fuel spends for the best horizontal position " should "be 2, for example " in {
    assert(TreacheryWhales.calculateFuel(List(16,1,2,0,4,2,7,1,2,14)) == 2)
  }

  "The fuel spends for the best horizontal position " should "be 2, for exercise " in {
    assert(TreacheryWhales.calculateFuel(Source.fromResource("TreacheryWhales1").getLines().next().split(",").map(_.toInt).toList) == 2)
  }
}
