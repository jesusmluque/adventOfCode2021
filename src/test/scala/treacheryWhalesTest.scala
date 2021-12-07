import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class TreacheryWhalesTest extends AnyFlatSpec {

  "The fuel spends for the best horizontal position " should "be 37, for example " in {
    assert(TreacheryWhales.calculateFuel(List(16,1,2,0,4,2,7,1,2,14)) == 37)
  }

  "The fuel spends for the best horizontal position " should "be 352997, for exercise " in {
    assert(TreacheryWhales.calculateFuel(Source.fromResource("TreacheryWhales1").getLines().next().split(",").map(_.toInt).toList) == 352997)
  }

  "The fuel spends for the best horizontal position taking into account method 2 " should "be 168, for example " in {
    assert(TreacheryWhales.calculateFuel2(List(16,1,2,0,4,2,7,1,2,14)) == 168)
  }

  "The fuel spends for the best horizontal position taking into account method 2 " should "be 101571302, for exercise " in {
    assert(TreacheryWhales.calculateFuel2(Source.fromResource("TreacheryWhales1").getLines().next().split(",").map(_.toInt).toList) == 101571302)
  }
}
