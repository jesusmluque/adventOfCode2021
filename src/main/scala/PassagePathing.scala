object PassagePathing {

  def calculateNumberOfPaths(connections: List[String]) =
    def findPaths(connectionPairs: List[(String, String)], nodeInit:String, currentPath:List[String], acc: Int): Int =
      if (nodeInit == "end")
        acc + 1
      else
        val nextPairs = connectionPairs.filter(p => p._1 == nodeInit || p._2 == nodeInit)
        nextPairs.foldLeft(acc) { (acc2, p) =>
          val node = if (p._1 != nodeInit)
              p._1
            else
              p._2

          node match {
            case c if c.toVector.forall(_.isUpper) => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2)
            case c if currentPath.contains(c) => acc2
            case c => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2)

          }
        }
    val pairOfConnections = connections.map { raw =>
      val nodes = raw.split("-")
      (nodes(0), nodes(1))
    }
    findPaths(pairOfConnections, "start", List(), 0)

  def calculateNumberOfPathsWithSingleTwice(connections: List[String]) =
    def findPaths(connectionPairs: List[(String, String)], nodeInit:String, currentPath:List[String], acc: Set[List[String]], lowerCaseTwice: String): Set[List[String]] =
      if (nodeInit == "end")
        acc + ("end" :: currentPath)
      else
        val nextPairs = connectionPairs.filter(p => p._1 == nodeInit || p._2 == nodeInit)
        nextPairs.foldLeft(acc) { (acc2, p) =>
          val node = if (p._1 != nodeInit)
            p._1
          else
            p._2
          node match {
            case c if c.toVector.forall(_.isUpper) => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, lowerCaseTwice)
            case c if (lowerCaseTwice == c) && c != "start" && c != "end" && currentPath.count(_ == c) < 2 => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, c)
            case c if currentPath.contains(c) => acc2
            case c => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, lowerCaseTwice)
          }
        }
    val pairOfConnections = connections.map { raw =>
      val nodes = raw.split("-")
      (nodes(0), nodes(1))
    }
    val lowerCaseTwice = pairOfConnections.filter(p => (!p._1.toVector.forall(_.isUpper) || !p._2.toVector.forall(_.isUpper)) && p._1 != "start" && p._2 != "start" && p._1 != "end" && p._2 != "end")
      .map(p => if (p._1.toVector.forall(_.isUpper)) p._2 else p._1)
    val totalPaths = lowerCaseTwice.foldLeft(Set[List[String]]()) { (acc, p) =>
      findPaths(pairOfConnections, "start", List(), acc, p)
    }
    totalPaths.size

}
