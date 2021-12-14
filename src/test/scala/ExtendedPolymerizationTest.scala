import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class ExtendedPolymerizationTest extends AnyFlatSpec {

  "After applying 10 steps, the result of taking the quantity of the most common element ans substracting the less common " should " be 1588 for the example " in {
    val raw = Source.fromResource("ExtendedPolymerization1").getLines().toList
    val template = raw.take(raw.indexOf("")).head
    val pairInsertionRawRules = raw.takeRight(raw.size - 1 - raw.indexOf(""))
    assert(ExtendedPolymerization.getDifferenceMostAndLessOccurrencies(template, pairInsertionRawRules, 10) == 1588L)
  }

  "After applying 10 steps, the result of taking the quantity of the most common element ans substracting the less common " should " be 3230 for the exercise" in {
    val raw = Source.fromResource("ExtendedPolymerization2").getLines().toList
    val template = raw.take(raw.indexOf("")).head
    val pairInsertionRawRules = raw.takeRight(raw.size - 1 - raw.indexOf(""))
    assert(ExtendedPolymerization.getDifferenceMostAndLessOccurrencies(template, pairInsertionRawRules, 10) == 3230L)
  }

  "After applying 40 steps, the result of taking the quantity of the most common element ans substracting the less common " should " be 2188189693529 for the example " in {
    val raw = Source.fromResource("ExtendedPolymerization1").getLines().toList
    val template = raw.take(raw.indexOf("")).head
    val pairInsertionRawRules = raw.takeRight(raw.size - 1 - raw.indexOf(""))
    assert(ExtendedPolymerization.getDifferenceMostAndLessOccurrencies(template, pairInsertionRawRules, 40) == 2188189693529L)
  }
}
