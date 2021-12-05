import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class HydrothermalTest extends AnyFlatSpec {

  "The number of points where at least two lines overlap " should "be 5 in the example " in {
    assert(Hydrothermal.calculatePointsOverlaped(Source.fromResource("Hydrothermal1").getLines().toList, false) == 5)
  }

  "The number of points where at least two lines overlap " should "be 5294 in the Exercise " in {
    assert(Hydrothermal.calculatePointsOverlaped(Source.fromResource("Hydrothermal2").getLines().toList, false) == 5294)
  }

  "The number of points where at least two lines overlap " should "be 12 in the example with diagonals " in {
    assert(Hydrothermal.calculatePointsOverlaped(Source.fromResource("Hydrothermal1").getLines().toList, true) == 12)
  }

  "The number of points where at least two lines overlap " should "be 21698 in the Exercise with diagonals " in {
    assert(Hydrothermal.calculatePointsOverlaped(Source.fromResource("Hydrothermal2").getLines().toList, true) == 21698)
  }
}
