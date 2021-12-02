import scala.annotation.tailrec

object Submarine {

  def calculateFinalPosition(instructions: List[String]):Int =
    @tailrec
    def calculatePositions(instructions: List[(String, Int)], acc:(Int, Int)): (Int, Int) = instructions match {
      case Nil => acc
      case head :: tail if head._1 == "forward" => calculatePositions(tail, (acc._1 + head._2, acc._2))
      case head :: tail if head._1 == "down" => calculatePositions(tail, (acc._1, acc._2 + head._2))
      case head :: tail if head._1 == "up" => calculatePositions(tail, (acc._1, acc._2 - head._2))
      case _ :: tail  => calculatePositions(tail, acc)
    }

    val parsedIns: List[(String, Int)] = parseInstructions(instructions)

    val result = calculatePositions(parsedIns, (0,0))
    result._1 * result._2

  private def parseInstructions(instructions: List[String]) = {
    val parsedIns = instructions.foldLeft(List[(String, Int)]()) { (acc, next) =>
      val pair = next.split(" ")
      (pair(0), pair(1).toInt) :: acc
    }
    parsedIns
  }

  def calculateFinalPositionWithAIM(instructions: List[String]): Int =
    def calculatePositions(instructions: List[(String, Int)], acc:(Int, Int, Int)): (Int, Int, Int) = instructions match {
      case Nil => acc
      case head :: tail if head._1 == "forward" => calculatePositions(tail, (acc._1 + head._2, acc._2 + acc._3 * head._2, acc._3))
      case head :: tail if head._1 == "down" => calculatePositions(tail, (acc._1, acc._2, acc._3 + head._2))
      case head :: tail if head._1 == "up" => calculatePositions(tail, (acc._1, acc._2, acc._3 - head._2))
      case _ :: tail  => calculatePositions(tail, acc)
    }

    val parsedIns: List[(String, Int)] = parseInstructions(instructions)

    val result = calculatePositions(parsedIns, (0,0,0))
    System.out.println(result)
    result._1 * result._2

}
