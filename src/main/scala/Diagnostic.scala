import scala.annotation.tailrec

object Diagnostic {

  def calculateRatesMultiplication(list: List[String]):Int =
    val input = parseData(list)
    val gamma = calculateRate(input, n => if (n.count(_ == 1) > n.count(_ == 0)) 1 else 0 )
    val epsilon = calculateRate(input, n => if (n.count(_ == 1) > n.count(_ == 0)) 0 else 1 )
    gamma * epsilon

  def parseData(raw: List[String]): Vector[List[Int]] =
    raw.foldLeft(Vector.fill(raw(0).size)(List[Int]())) { (acc, next) =>
      val values = next.split("").map(_.toInt).zipWithIndex.toVector
      values.map(n => n._1 :: acc(n._2))
    }
    
  def calculateRate(input: Vector[List[Int]], f:List[Int] => Int) =
    Integer.parseInt(input.map(f).mkString(""), 2)

  def calculateSupportRatesMultiplication(list: List[String]): Int =
    @tailrec
    def filter(list: List[String], index: Int, condition: List[Int] => Int):Int =
      if (list.size == 1)
        Integer.parseInt(list(0), 2)
      else
        val input = parseData(list)
        val filteredList = list.filter { n =>
          val comp = condition(input(index))
          n.toCharArray()(index) == comp.toString.toCharArray()(0)
        }
        filter(filteredList, index + 1, condition)

    val oxigen = filter(list, 0, l => if (l.count(_ == 1) >= l.count(_ == 0)) 1 else 0)
    val co2 = filter(list, 0, l => if (l.count(_ == 1) < l.count(_ == 0)) 1 else 0)

    oxigen * co2
}
