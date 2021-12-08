object SevenSegment {

  def countOnlyOutputDigitsForUniqueNumberOfSegment(input: List[String]): Int =
    def countWithAcc(input:  List[List[String]], acc: Int):Int = input match {
      case Nil => acc
      case List(_,_) :: tail => countWithAcc(tail, acc + 1)
      case List(_,_,_) :: tail => countWithAcc(tail, acc + 1)
      case List(_,_,_,_) :: tail => countWithAcc(tail, acc + 1)
      case List(_,_,_,_,_,_,_) :: tail => countWithAcc(tail, acc + 1)
      case _ :: tail =>  countWithAcc(tail, acc)
    }
    countWithAcc(parseData(input), 0)


  def parseData(input: List[String]) =
    input.foldLeft(List[List[String]]()) { (acc, entry) =>
      val splitted = entry.split(" \\| ")(1)
      splitted.split(" ").map(_.split("").toList).toList ++ acc
    }

  def addAllOutputNumbers(input: List[String]) =
    val data = parseData2(input)
    data.foldLeft(0) { (acc, n) =>
      acc + decodeRow(n)
    }

  def parseData2(input: List[String]) =
    input.foldLeft(List[(List[Set[String]], Vector[Set[String]])]()) { (acc, entry) =>
      val splitted = entry.split(" \\| ")
      (splitted(0).split(" ").map(_.split("").toSet).toList, splitted(1).split(" ").map(_.split("").toSet).toVector) :: acc
    }

  def decodeRow(row: (List[Set[String]], Vector[Set[String]])) =
    val digits = row._1.sortBy(_.size)
    val decodedDigits: Map[Int, Set[String]] = digits.foldLeft(Map[Int, Set[String]]()) { (acc, next) =>
      if (next.size == 2)
        acc.updated(1, next)
      else if (next.size == 3)
        acc.updated(7, next)
      else if (next.size == 4)
        acc.updated(4, next)
      else if (next.size == 7)
        acc.updated(8, next)
      else if (next.size == 6 && !acc(1).subsetOf(next))
        acc.updated(6, next)
      else if (next.size == 5 && acc(1).subsetOf(next))
        acc.updated(3, next)
      else if (next.size == 6 && acc(3).subsetOf(next))
        acc.updated(9, next)
      else if (next.size == 6 && !acc(3).subsetOf(next))
        acc.updated(0, next)
      else if (next.size == 5 && acc(4).intersect(next).size == 3)
        acc.updated(5, next)
      else
        acc.updated(2, next)
    }

    val number = row._2
    val rawNumber = number.map { digit =>
      decodedDigits.find(_._2 == digit).map(_._1).get
    }
    rawNumber.mkString.toInt
}
