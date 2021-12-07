object TreacheryWhales {

  def calculateFuel(positions: List[Int]) =
    positions.map { next =>
      positions.foldLeft(0) { (acc,n) =>
        acc + (next - n) * (if (next < n) -1 else 1)
      }
    }.min

  def calculateFuel2(positions: List[Int]) =
    (0 to positions.max).map { next =>
      positions.foldLeft(0) { (acc, n) =>
        acc + (1 to ((next - n) * (if (next < n) -1 else 1))).sum
      }
    }.min
}
