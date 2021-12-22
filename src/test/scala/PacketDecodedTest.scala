import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class PacketDecodedTest extends AnyFlatSpec {

  "Adding up all the version numbers of all packet for the example 8A004A801A8002F478 " should " be 16 " in {
    assert(PacketDecoded.getTotalVersion("8A004A801A8002F478") == 16)
  }

  "Adding up all the version numbers of all packet for the example 620080001611562C8802118E34 " should " be 12 " in {
    assert(PacketDecoded.getTotalVersion("620080001611562C8802118E34") == 12)
  }

  "Adding up all the version numbers of all packet for the example C0015000016115A2E0802F182340 " should " be 23 " in {
    assert(PacketDecoded.getTotalVersion("C0015000016115A2E0802F182340") == 23)
  }

  "Adding up all the version numbers of all packet for the example A0016C880162017C3686B18A3D4780 " should " be 31 " in {
    assert(PacketDecoded.getTotalVersion("A0016C880162017C3686B18A3D4780") == 31)
  }

  "Adding up all the version numbers of all packet for the exercise " should " be 979" in {
    assert(PacketDecoded.getTotalVersion(Source.fromResource("PacketDecoded1").getLines().next) == 979)
  }

  "The evaluation of the exression contained into the transmission C200B40A82 " should " be 3" in {
    assert(PacketDecoded.evaluateExpression2("C200B40A82") == 3)
  }

  "The evaluation of the exression contained into the transmission 04005AC33890 " should " be 54" in {
    assert(PacketDecoded.evaluateExpression2("04005AC33890") == 54)
  }

  "The evaluation of the exression contained into the transmission 880086C3E88112 " should " be 7" in {
    assert(PacketDecoded.evaluateExpression2("880086C3E88112") == 7)
  }

  "The evaluation of the exression contained into the transmission CE00C43D881120 " should " be 9" in {
    assert(PacketDecoded.evaluateExpression2("CE00C43D881120") == 9)
  }

  "The evaluation of the exression contained into the transmission D8005AC2A8F0 " should " be 1" in {
    assert(PacketDecoded.evaluateExpression2("D8005AC2A8F0") == 1)
  }

  "The evaluation of the exression contained into the transmission F600BC2D8F " should " be 0" in {
    assert(PacketDecoded.evaluateExpression2("F600BC2D8F") == 0)
  }

  "The evaluation of the exression contained into the transmission 9C005AC2F8F0 " should " be 0" in {
    assert(PacketDecoded.evaluateExpression2("9C005AC2F8F0") == 0)
  }

  "The evaluation of the exression contained into the transmission 9C0141080250320F1802104A08 " should " be 1" in {
    assert(PacketDecoded.evaluateExpression2("9C0141080250320F1802104A08") == 1)
  }

  "The evaluation of the exression contained into the transmission exercise " should " be 1" in {
    assert(PacketDecoded.evaluateExpression2(Source.fromResource("PacketDecoded1").getLines().next) == 1L)
  }
}
