import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class TransparentOrigamiTest extends AnyFlatSpec {

  "The number of dots that are visible after completing just the first fold instruction " should " be 17 in example " in {
    val raw = Source.fromResource("TransparentOrigami1").getLines().toList
    val paper = raw.take(raw.indexOf(""))
    val instructions = raw.takeRight(raw.size - 1 - raw.indexOf(""))
    assert(TransparentOrigami.applyFoldInstructionFromRawData(paper, instructions.head) == 17)
  }

  "The number of dots that are visible after completing just the first fold instruction in exmample 3" should " be 16 " in {
    val raw = Source.fromResource("TransparentOrigami3").getLines().toList
    val paper = raw.take(raw.indexOf(""))
    val instructions = raw.takeRight(raw.size - 1 - raw.indexOf(""))
    assert(TransparentOrigami.applyFoldInstructionFromRawData(paper, instructions.head) == 16)
  }

  "The number of dots that are visible after completing just the first fold instruction in exercise" should " be 671 " in {
    val raw = Source.fromResource("TransparentOrigami2").getLines().toList
    val paper = raw.take(raw.indexOf(""))
    val instructions = raw.takeRight(raw.size - 1 - raw.indexOf(""))
    assert(TransparentOrigami.applyFoldInstructionFromRawData(paper, instructions.head) == 671)
  }

  "The number " should "be" in {
    val raw = Source.fromResource("TransparentOrigami2").getLines().toList
    val paper = raw.take(raw.indexOf(""))
    val instructions = raw.takeRight(raw.size - 1 - raw.indexOf(""))
    TransparentOrigami.decode(paper, instructions)
  }
}
