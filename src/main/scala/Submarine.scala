import scala.annotation.tailrec

object Submarine {

  def calculateFinalPosition(instructions: List[String]):Int =
    @tailrec
    def calculatePositions(instructions: List[(String, Int)], acc:(Int, Int)): (Int, Int) = instructions match
      case Nil => acc
      case ("forward", value) :: tail => calculatePositions(tail, (acc._1 + value, acc._2))
      case ("down", value) :: tail => calculatePositions(tail, (acc._1, acc._2 + value))
      case ("up", value) :: tail => calculatePositions(tail, (acc._1, acc._2 - value))
      case _ :: tail  => calculatePositions(tail, acc)


    val parsedIns: List[(String, Int)] = parseInstructions(instructions)

    val result = calculatePositions(parsedIns, (0,0))
    result._1 * result._2

  def calculateFinalPositionWithAIM(instructions: List[String]): Int =
    @tailrec
    def calculatePositions(instructions: List[(String, Int)], acc:(Int, Int, Int)): (Int, Int, Int) = instructions match
      case Nil => acc
      case ("forward", value) :: tail => calculatePositions(tail, (acc._1 + value, acc._2 + (acc._3 * value), acc._3))
      case ("down", value) :: tail => calculatePositions(tail, (acc._1, acc._2, acc._3 + value))
      case ("up", value) :: tail => calculatePositions(tail, (acc._1, acc._2, acc._3 - value))
      case _ :: tail  => calculatePositions(tail, acc)


    val parsedIns: List[(String, Int)] = parseInstructions(instructions)

    val result = calculatePositions(parsedIns, (0,0,0))
    result._1 * result._2

  private def parseInstructions(instructions: List[String]) =
    instructions.foldLeft(List[(String, Int)]()) { (acc, next) =>
      val pair = next.split(" ")
      (pair(0), pair(1).toInt) :: acc
    }.reverse

}
