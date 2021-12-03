import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class DiagnosticTest extends AnyFlatSpec {

  "multiplying epsilon and gamma rate for the Example 1 " should " be 198 " in {
    assert(Diagnostic.calculateRatesMultiplication(Source.fromResource("Diagnostic1").getLines().toList) == 198 )
  }

  "multiplying epsilon and gamma rate for the Exercise " should " be 3633500 " in {
    assert(Diagnostic.calculateRatesMultiplication(Source.fromResource("Diagnostic2").getLines().toList) == 3633500 )
  }
}
