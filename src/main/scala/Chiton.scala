import scala.collection.mutable

object Chiton {

  def findLowestRiskPath(raw: List[String], times: Int) =
    val matrix = raw.zipWithIndex.foldLeft(Vector.fill(raw.length)(Vector.fill(raw.head.length)(0))) { (acc, row) =>
      acc.updated(row._2, row._1.split("").map(_.toInt).toVector)
    }

    def getNeihgbors(node: (Int, Int)) =
      List((node._1 + 1, node._2), (node._1, node._2 + 1), (node._1 - 1, node._2), (node._1, node._2 - 1))
        .filter(p => p._1 >= 0 && p._2 >= 0 && p._1 < (matrix.length * times) && p._2 < (matrix.head.length * times) )

    def traverse(queue: mutable.PriorityQueue[((Int, Int), Int)], visited: Set[(Int, Int)], endNode: Int):Int  =
      if (queue.isEmpty)
        endNode
      else
        val node@(next, risk) = queue.dequeue
        if (next == (matrix.length * times - 1, matrix.head.length * times - 1))
          risk
        else if (visited.contains(next))
          traverse(queue, visited, endNode)
        else
          val neighbors = getNeihgbors(next)
          neighbors.foreach { n =>
            val newX = n._1 % matrix.length
            val newY = n._2 % matrix.head.length
            val newValue = 1 + (matrix(newX)(newY) + (n._1 / matrix.length) + (n._2 / matrix.head.length) - 1) % 9
            queue.addOne((n, newValue + risk))
          }
          traverse(queue, visited + next, endNode)

    val visited = Set[(Int, Int)]()
    val queue = mutable.PriorityQueue(((0,0), 0))(Ordering.by(-_._2))

    traverse(queue, visited, -1)
}
