object PassagePathing {
  type PATHS = Set[List[String]]
  type Connection = (String, String)
  private final val END_NODE = "end"
  private final val START_NODE = "start"

  def calculateNumberOfPaths(connections: List[String]) =
    def findPaths(connectionPairs: List[Connection], nodeInit:String, currentPath:List[String], acc: Int): Int =
      if (nodeInit == END_NODE)
        acc + 1
      else
        val nextPairs = getNextConnectionsFor(connectionPairs, nodeInit)
        nextPairs.foldLeft(acc) { (acc2, p) =>
          val node = selectNodeFromConnection(nodeInit, p)
          node match {
            case c if isNodeUpperCase(c) => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2)
            case c if currentPath.contains(c) => acc2
            case c => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2)
          }
        }
    val pairOfConnections = parseConnection(connections)
    findPaths(pairOfConnections, START_NODE, List(), 0)

  def calculateNumberOfPathsWithSingleTwice2(connections: List[String]) =
    def findPaths(connectionPairs: List[Connection], nodeInit:String, currentPath:List[String], acc: Int, repeated: Boolean): Int =
      if (nodeInit == END_NODE)
        acc + 1
      else
        val nextPairs = getNextConnectionsFor(connectionPairs, nodeInit)
        nextPairs.foldLeft(acc) { (acc2, p) =>
          val node = selectNodeFromConnection(nodeInit, p)
          node match {
            case c if isNodeUpperCase(c) => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, repeated)
            case c if !repeated && c != START_NODE && currentPath.contains(c) => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, true)
            case c if currentPath.contains(c) => acc2
            case c => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, repeated)
          }
        }
    val pairOfConnections = parseConnection(connections)
    findPaths(pairOfConnections, START_NODE, List(), 0, false)

  private def parseConnection(connections: List[String]) = {
    connections.map { raw =>
      val nodes = raw.split("-")
      (nodes(0), nodes(1))
    }
  }

  def calculateNumberOfPathsWithSingleTwice(connections: List[String]) =
    def findPaths(connectionPairs: List[Connection], nodeInit:String, currentPath:List[String], acc: PATHS, lowerCaseTwice: String): PATHS =
      if (nodeInit == END_NODE)
        acc + (END_NODE :: currentPath)
      else
        val nextPairs = getNextConnectionsFor(connectionPairs, nodeInit)
        nextPairs.foldLeft(acc) { (acc2, p) =>
          val node = selectNodeFromConnection(nodeInit, p)
          node match {
            case c if isNodeUpperCase(c) => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, lowerCaseTwice)
            case c if (lowerCaseTwice == c) && c != START_NODE && c != END_NODE && currentPath.count(_ == c) < 2 => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, c)
            case c if currentPath.contains(c) => acc2
            case c => findPaths(connectionPairs, c, nodeInit :: currentPath, acc2, lowerCaseTwice)
          }
        }
    val pairOfConnections = parseConnection(connections)
    val lowerCaseTwice = pairOfConnections.filter(p => (!isNodeUpperCase(p._1) || !isNodeUpperCase(p._2)) && p._1 != START_NODE && p._2 != START_NODE && p._1 != END_NODE && p._2 != END_NODE)
      .map(p => if (isNodeUpperCase(p._1)) p._2 else p._1)
    lowerCaseTwice.foldLeft(Set[List[String]]()) { (acc, p) =>
      findPaths(pairOfConnections, START_NODE, List(), acc, p)
    }.size

  private def isNodeUpperCase(c: String) = {
    c.toVector.forall(_.isUpper)
  }

  private def selectNodeFromConnection(nodeInit: String, p: Connection) = {
    if (p._1 != nodeInit)
      p._1
    else
      p._2
  }

  private def getNextConnectionsFor(connectionPairs: List[Connection], node: String) = {
    connectionPairs.filter(p => p._1 == node || p._2 == node)
  }
}
