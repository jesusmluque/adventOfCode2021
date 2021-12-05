object GiantSquid {

  def winnerScore(input: List[String]):Int =
    val (values, boards) = parseBoards(splitData(input, List(), List()))
    execute(boards, values)

  def execute(boards: List[List[List[Int]]], values: List[Int]):Int =
    val eval = evaluation(boards, values.head)
    if (eval._2 == -1)
      execute(eval._1, values.tail)
    else
      eval._2

  def winnerScoreLast(input: List[String]):Int =
    val (values, boards) = parseBoards(splitData(input, List(), List()))
    execute2(boards, values, false)

  def execute2(boards: List[List[List[Int]]], values: List[Int], onlyOne: Boolean):Int =
    val eval = evaluation(boards, values.head)
    System.out.println(eval._1.size)
    if (eval._1.size > 1)
      execute2(eval._1, values.tail, onlyOne)
    else if (onlyOne && eval._2 != -1)
      eval._2
    else {
      execute2(eval._1, values.tail, true)
    }

  def findWinnerByRow(newBoards: List[List[List[Int]]]) = {
    newBoards.map(b => (b.map(row => row.forall(_ == -1)).reduce((a, b) => a || b), b)).filter(_._1)
  }

  def changeRowsByCols(newBoards: List[List[List[Int]]]) = {
    newBoards.map { board =>
      board.foldLeft(List.fill(board.size)(List[Int]())) { (acc, row) =>
        acc.zipWithIndex.map(col => row(col._2) :: col._1)
      }
    }
  }

  def calculateWinnerBoardScore(value: Int, newBoards: List[List[List[Int]]], boardWithV: List[(Boolean, List[List[Int]])]) = {
    (newBoards.filter(a => boardWithV.forall(b => b._2 != a)), boardWithV.map(b => b._2.foldLeft(0) { (acc, row) =>
      row.filter(_ != -1).sum + acc
    }).head * value)
  }

  def evaluation(boards: List[List[List[Int]]], value: Int) =
    val newBoards = boards.map { board =>
      board.map(b => b.map(elem => if (elem == value) -1 else elem))
    }
    System.out.println(newBoards.size)

    val boardWithV = findWinnerByRow(changeRowsByCols(newBoards))
    if (boardWithV.nonEmpty)
      calculateWinnerBoardScore(value, newBoards, boardWithV)
    else
      val boardWithH = findWinnerByRow(newBoards)
      if (boardWithH.nonEmpty)
        calculateWinnerBoardScore(value, newBoards, boardWithH)
      else
        (newBoards, -1)

  def splitData(raw: List[String], acc:List[List[String]], acc2: List[String]):List[List[String]] = raw match {
    case Nil => acc2 :: acc
    case "" :: tail => splitData(tail, acc2 :: acc, List())
    case head :: tail => splitData(tail, acc, head :: acc2)
  }

  def parseBoards(rawBoards: List[List[String]]) =
    rawBoards.foldLeft((List[Int](), List[List[List[Int]]]())) { (acc, b) =>
      if (b.size == 1) {
        (b.flatMap(_.split(",").toList.map(_.toInt)), acc._2)
      } else {
        (acc._1, b.map(_.split(" ").toList.filter(_ != "").map(_.trim).map(_.toInt)) :: acc._2)
      }
    }
}
