import scala.annotation.tailrec

object SmokeBasin {

  case class SmokeBasinField(private val field: Vector[Vector[Int]]) {
    def get(coordinates: (Int, Int)): Int = field(coordinates._1)(coordinates._2)

    def getNeighbors(coordinates: (Int, Int)): List[(Int, Int)] =
      val indexLastXElement = field.size - 1
      val indexLastYElement = field.head.size - 1
      coordinates match {
        case (0, 0) => (0, 1) :: (1, 0) :: Nil
        case (0, y) if y == indexLastYElement => (1, indexLastYElement) :: (0, indexLastYElement - 1) :: Nil
        case (0, y) => (1, y) :: (0, y - 1) :: (0, y + 1) :: Nil
        case (x, 0) if x == indexLastXElement => (indexLastXElement, 1) :: (indexLastXElement - 1, 0) :: Nil
        case (x, 0) => (x + 1, 0) :: (x - 1, 0) :: (x, 1) :: Nil
        case (x, y) if x == indexLastXElement && y == indexLastYElement => (indexLastXElement, indexLastYElement - 1) :: (indexLastXElement - 1, indexLastYElement) :: Nil
        case (x, y) if x == indexLastXElement => (indexLastXElement, y - 1) :: (indexLastXElement, y + 1) :: (indexLastXElement - 1, y) :: Nil
        case (x, y) if y == indexLastYElement => (x - 1, indexLastYElement) :: (x + 1, indexLastYElement) :: (x, indexLastYElement - 1) :: Nil
        case (x, y) => (x + 1, y) :: (x - 1, y) :: (x, y + 1) :: (x, y - 1) :: Nil
      }

    def foldField[A](acc: A)(f: (A, Int, Int) => A) =
      this.field.zipWithIndex.foldLeft(acc) { (acc1, row) =>
        row._1.zipWithIndex.foldLeft(acc1) { (acc2, point) =>
          f(acc2, row._2, point._2)
        }
      }
  }
  object SmokeBasinField {
    def apply(raw: List[String]):SmokeBasinField =
      SmokeBasinField(raw.zipWithIndex.foldLeft(Vector.fill(raw.length)(Vector.fill(raw.head.length)(0)))((acc,row) => acc.updated(row._2, row._1.split("").map(_.toInt).toVector)))
  }

  def findRiskLevelFor(input: List[String]) =
    val field = SmokeBasinField(input)
    field.foldField(0) {(acc:Int, x:Int, y:Int) =>
      if (isALowPoint(x, y, field))
        field.get(x, y) + 1 + acc
      else
        acc
    }

  def findProductOfThreeLargestBasinsFor(input: List[String]) =
    def findBasinMembers(field: SmokeBasinField, basin: (Int, Int), acc: Set[(Int, Int)]): Set[(Int, Int)] =
      if (acc.contains(basin))
        acc
      else if (field.get(basin) == 9)
        acc
      else
        field.getNeighbors(basin).foldLeft(acc) { (acc2, p) =>
          if (field.get(p) != 9)
            findBasinMembers(field, p, acc2 + basin)
          else
            acc2
        }
    val field = SmokeBasinField(input)
    val basins = field.foldField(List[(Int, Int)]()) { (acc, x, y) =>
      if (isALowPoint(x, y, field))
        (x, y) :: acc
      else
        acc
    }
    basins.map(findBasinMembers(field, _, Set())).sortBy(_.size).takeRight(3).map(_.size).product

  def isALowPoint(x: Int, y: Int, field: SmokeBasinField) = {
    val valuesOfNeighbors = field.getNeighbors((x, y)).map(field.get)
    valuesOfNeighbors.count(_ <= field.get(x,y)) == 0
  }
}
