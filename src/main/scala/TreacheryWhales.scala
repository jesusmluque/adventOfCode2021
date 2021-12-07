object TreacheryWhales {

  def calculateFuel(positions: List[Int]) =
    val positionSorted = positions.sorted
    val mediana = positionSorted( if (positionSorted.size % 2 == 0) (positionSorted(positionSorted.size / 2) + positionSorted((positionSorted.size / 2 + 1))) / 2 else (positionSorted(positionSorted.size) / 2) + 1)
    val map = positions.foldLeft(Map[Int,Int]()) { (acc, next) =>
      acc.updated(next, acc.getOrElse(next, 0) + 1)
    }
    System.out.println(map)
    mediana
}
