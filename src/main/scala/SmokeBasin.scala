object SmokeBasin {

  case class SmokeBasinField(vector: Vector[Int], rowSize: Int)
  object SmokeBasinField {
    def apply(raw: List[String]):SmokeBasinField =
      SmokeBasinField(raw.foldLeft(Vector[Int]())((acc,row) => row.split("").toVector.map(_.toInt) ++ acc), raw.head.length)
  }

  def findRiskLevelFor(input: List[String]) =
    val field = SmokeBasinField(input)
    field.vector.zipWithIndex.foldLeft(0) { (acc, smoke) =>
      if ((smoke._2 % field.rowSize - 1 > 0 && field.vector(smoke._2 - 1) < smoke._1) &&
        (smoke._2 % field.rowSize + 1 < field.rowSize && field.vector(smoke._2 + 1) < smoke._1)  &&
        (smoke._2 - field.rowSize > 0   && field.vector(smoke._2 - field.rowSize) < smoke._1) &&
          (smoke._2 - field.rowSize > field.vector.length  && field.vector(smoke._2 + field.rowSize) < smoke._1)) {
        acc + smoke._1 + 1
      } else
        acc
    }
}
