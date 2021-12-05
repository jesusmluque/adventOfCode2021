object Hydrothermal {

  def calculatePointsOverlaped(input: List[String]) = {
    val lines = input.map{ rawLine =>
      val points = rawLine.split("->")
      val init = points(0).split(",")
      val end = points(1).split(",")
      ((init(0).trim.toInt, init(1).trim.toInt),(end(0).trim.toInt, end(1).trim.toInt))
    }

    lines.foldLeft(Map[(Int,Int), Int]()) { (acc, next) =>
      if (next._1._1 == next._2._1)
        val s = if (next._1._2 > next._2._2) -1 else 1
        Range(next._1._2, next._2._2 + s, s).map((next._1._1, _)).foldLeft(acc) { (acc2, n) =>
          acc2.updated(n, acc2.getOrElse(n, 0) + 1)
        }
      else if (next._1._2 == next._2._2)
        val s = if (next._1._1 > next._2._1) -1 else 1
        Range(next._1._1, next._2._1 + s, s).map((_, next._1._2)).foldLeft(acc) { (acc2, n) =>
          acc2.updated(n, acc2.getOrElse(n, 0) + 1)
        }
      else
        acc
    }.values.count(_ >= 2)
  }
}