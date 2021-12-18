import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class ChitonTest extends AnyFlatSpec {

  "The lowest total risk of any path from top left to bottom right for the example " should " be 40 " in {
    assert(Chiton.findLowestRiskPath(Source.fromResource("Chiton1").getLines().toList, 1) == 40)
  }

  "The lowest total risk of any path from top left to bottom right for the exercise " should " be 652 " in {
    assert(Chiton.findLowestRiskPath(Source.fromResource("Chiton2").getLines().toList, 1) == 652)
  }

  "The lowest total risk of any path from top left to bottom right for the 5 times example grid " should " be 315 " in {
    assert(Chiton.findLowestRiskPath(Source.fromResource("Chiton1").getLines().toList, 5) == 315)
  }

  "The lowest total risk of any path from top left to bottom right for the 5 times exercise grid " should " be 2938 " in {
    assert(Chiton.findLowestRiskPath(Source.fromResource("Chiton2").getLines().toList, 5) == 2938)
  }
}
