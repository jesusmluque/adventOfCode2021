import org.scalatest.flatspec.AnyFlatSpec

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
}
