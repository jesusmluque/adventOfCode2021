import scala.annotation.tailrec

object SmokeBasin {
  type Coordinates = (Int, Int)
  type Matrix[A] = Vector[Vector[A]]

  case class SmokeBasinField[A](private val field: Matrix[A]) {

    def get(coordinates: Coordinates): A = field(coordinates._1)(coordinates._2)

    def getNeighbors(coordinates: Coordinates): List[Coordinates] =
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

    def foldField[B](acc: B)(f: (B, Coordinates) => B) =
      this.field.zipWithIndex.foldLeft(acc) { (acc1, row) =>
        row._1.zipWithIndex.foldLeft(acc1) { (acc2, point) =>
          f(acc2, (row._2, point._2))
        }
      }
  }
  object SmokeBasinField {
    def apply(raw: List[String]):SmokeBasinField[Int] = {
      val initialVector = Vector.fill(raw.length)(Vector.fill(raw.head.length)(0))
      SmokeBasinField(raw.zipWithIndex.foldLeft(initialVector) { (acc, row) =>
        acc.updated(row._2, row._1.split("").map(_.toInt).toVector)
      })
    }
  }

  def findRiskLevelFor(input: List[String]) =
    val field = SmokeBasinField(input)
    field.foldField(0) {(acc:Int, point) =>
      if (isALowPoint(point, field))
        field.get(point) + 1 + acc
      else
        acc
    }

  def findProductOfThreeLargestBasinsFor(input: List[String]) =
    def findBasinMembers(field: SmokeBasinField[Int], basin: Coordinates, acc: Set[Coordinates]): Set[Coordinates] =
      if (acc.contains(basin))
        acc
      else if (field.get(basin) == 9)
        acc
      else
        field.getNeighbors(basin).foldLeft(acc) { (acc2, p) =>
          findBasinMembers(field, p, acc2 + basin)
        }
    val field = SmokeBasinField(input)
    val basins = field.foldField(List[Coordinates]()) { (acc, point) =>
      if (isALowPoint(point, field))
        point :: acc
      else
        acc
    }
    basins.map(findBasinMembers(field, _, Set())).sortBy(_.size).takeRight(3).map(_.size).product

  def isALowPoint(point: Coordinates, field: SmokeBasinField[Int]) = {
    val valuesOfNeighbors = field.getNeighbors(point).map(field.get)
    valuesOfNeighbors.count(_ <= field.get(point)) == 0
  }
}
