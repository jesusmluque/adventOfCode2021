object TreacheryWhales {

  def calculateFuel(positions: List[Int]) =
    positions.map { next =>
      positions.foldLeft(0) { (acc,n) =>
        acc + subtract(next, n)
      }
    }.min

  def calculateFuel2(positions: List[Int]) =
    (0 to positions.max).map { next =>
      positions.foldLeft(0) { (acc, n) =>
        acc + (1 to subtract(next, n)).sum
      }
    }.min

  def subtract(x: Int, y: Int) = (x - y) * (if (x < y) -1 else 1)
}
