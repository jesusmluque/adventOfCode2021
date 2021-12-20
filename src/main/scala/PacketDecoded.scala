object PacketDecoded {

  val hexadecimalToBinary = Map(
    "0" -> "0000",
    "1" -> "0001",
    "2" -> "0010",
    "3" -> "0011",
    "4" -> "0100",
    "5" -> "0101",
    "6" -> "0110",
    "7" -> "0111",
    "8" -> "1000",
    "9" -> "1001",
    "A" -> "1010",
    "B" -> "1011",
    "C" -> "1100",
    "D" -> "1101",
    "E" -> "1110",
    "F" -> "1111"
  )

  def fromHexToBinary(hexadecimal: String) = hexadecimal.split("").flatMap { c =>
    hexadecimalToBinary(c).split("")
  }.toList

  def getTotalVersion(rawTransmission: String) =
    def calculateTotalVersion(transmission: List[String], acc: Int): Int =
      if (transmission.isEmpty)
        acc
      else
        val (version, rest) = transmission.splitAt(3)
        rest.splitAt(3) match {
          case (List("1", "0", "0"), _) =>
            acc + Integer.parseInt(version.mkString, 2)
          case (List(_, _, _), lengthType :: rest) if lengthType == "0" =>
            val (subPacketLength, r) = rest.splitAt(15)
            val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
            calculateTotalVersion(r.take(decimalLength), Integer.parseInt(version.mkString, 2) + acc)
          case (List(_, _, _), lengthType :: rest) if lengthType == "1" =>
            val (subPacketLength, r) = rest.splitAt(11)
            val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
            calculateTotalVersion(r, Integer.parseInt(version.mkString, 2) + acc)
          case _ => acc
        }

    val transmission = fromHexToBinary(rawTransmission)
    calculateTotalVersion(transmission, 0)


}
