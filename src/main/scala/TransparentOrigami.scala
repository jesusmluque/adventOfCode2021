object TransparentOrigami {

  def applyFoldInstructionFromRawData(paper: List[String], instruction: String) =
    val instructionParsed: (String, Int) = parseInstruction(instruction)
    val paperParsed = parsePaper(paper)
    val res = applyFoldInstruction(instructionParsed, paperParsed)
    res.length

  def decode(paper: List[String], instructions: List[String]) =
    val parsedPaper = parsePaper(paper)
    val parseInstructions = instructions.map(parseInstruction)
    val finalPaper = parseInstructions.foldLeft(parsedPaper) { (acc, n) =>
      applyFoldInstruction(n, acc)
    }
    paperToString(finalPaper)

  private def paperToString(paper: List[(String, String)]) =
    val maxX = paper.map(a => (a._1.toInt, a._2.toInt)).maxBy(_._1)._1
    val maxY = paper.map(a => (a._1.toInt, a._2.toInt)).maxBy(_._2)._2

    for y <- 0 to maxY do
      for x <- 0 to maxX do
        if (paper.contains((x.toString, y.toString)))
          print("#")
        else
          print(".")
      print("\n")

  private def parsePaper(paper: List[String]) = {
    val paperParsed = paper.map(s => (s.split(",")(0), s.split(",")(1)))
    paperParsed
  }

  private def parseInstruction(instruction: String) = {
    val instructionParsed = instruction.split(" ").last.split("=") match {
      case Array(variable, value) => (variable, value.toInt)
    }
    instructionParsed
  }

  private def applyFoldInstruction(instructionParsed: (String, Int), paperParsed: List[(String, String)]) = {
    instructionParsed match {
      case ("y", y) =>
        val map = paperParsed.groupBy(_._2.toInt < y)
        val (Some(up), Some(down)) = (map.get(true), map.get(false))
        down.foldLeft(up) { (acc, d) =>
          if (!acc.contains((d._1, (2 * y - d._2.toInt).toString)))
            (d._1, (2 * y - d._2.toInt).toString) :: acc
          else
            acc
        }
      case ("x", x) =>
        val map = paperParsed.groupBy(_._1.toInt < x)
        val (Some(up), Some(down)) = (map.get(true), map.get(false))
        down.foldLeft(up) { (acc, d) =>
          if (!acc.contains(((2 * x - d._1.toInt).toString, d._2)))
            ((2 * x - d._1.toInt).toString, d._2) :: acc
          else
            acc
        }
    }
  }

}
