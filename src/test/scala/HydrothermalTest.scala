import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class HydrothermalTest extends AnyFlatSpec {

  "The number of points where at least two lines overlap " should "be 5 in the example " in {
    assert(Hydrothermal.calculatePointsOverlaped(Source.fromResource("Hydrothermal1").getLines().toList) == 5)
  }

  "The number of points where at least two lines overlap " should "be 5 in the Exercise " in {
    assert(Hydrothermal.calculatePointsOverlaped(Source.fromResource("Hydrothermal2").getLines().toList) == 5)
  }
}
