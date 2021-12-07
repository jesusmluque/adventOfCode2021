object Lanternfish {

  def calculate(fishes: List[Int], days: Int) =

    def parseFishes(raw: List[Int], acc: Map[Int, Long]):Map[Int, Long] = raw match
      case Nil => acc
      case head :: tail => parseFishes(tail, acc.updated(head, acc.getOrElse(head, 0L) + 1L))

    def count(days: Int, lanterns: Map[Int, Long]):Map[Int, Long] =
      if (days == 0)
        lanterns
      else
        val newLanterns = (0 to 8).foldLeft((lanterns, 0L)) { (acc, daysUntilBorn) =>
          daysUntilBorn match {
            case 0 =>
              val readyToBorn = acc._1.getOrElse(0, 0L)
              if (readyToBorn > 0L)
                (acc._1.updated(0, 0L), readyToBorn)
              else
                acc
            case n =>
              val amountOfLanternsInN = acc._1.getOrElse(n, 0L)
              if (amountOfLanternsInN > 0)
                (acc._1.updated(n, 0L).updated(n - 1, amountOfLanternsInN + acc._1.getOrElse(n-1, 0L)), acc._2)
              else
                acc
          }
        }
        val lanternsWithNewMembers = newLanterns._1.updated(8, newLanterns._2).updated(6, newLanterns._2 + newLanterns._1.getOrElse(6, 0L))
        count(days - 1, lanternsWithNewMembers)

    val res = count(days, parseFishes(fishes, Map()))
    res.foldLeft(0L)((acc, n) => acc + n._2)
}
