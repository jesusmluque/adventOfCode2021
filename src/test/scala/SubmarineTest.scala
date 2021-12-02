import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class SubmarineTest extends AnyFlatSpec {

  "The final depth and horizontal position multiplied " should " be 150 in Exemple1 " in {
    assert(Submarine.calculateFinalPosition(Source.fromResource("Submarine1").getLines().toList) == 150)
  }

  "The final depth and horizontal position multiplied " should " be 1660158 in Exercise file " in {
    assert(Submarine.calculateFinalPosition(Source.fromResource("Submarine2").getLines().toList) == 1660158)
  }

  "The final depth and horizontal position multiplied using aim " should " be 900 in Exemple1 " in {
    assert(Submarine.calculateFinalPositionWithAIM(Source.fromResource("Submarine1").getLines().toList) == 900)
  }
}
