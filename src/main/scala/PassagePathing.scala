object PassagePathing {
  type Connection = (String, String)

  private final val END_NODE = "end"
  private final val START_NODE = "start"

  def calculateNumberOfPaths(rawConnections: List[String]) =
    def findPaths(connectionPairs: List[Connection], fromNode:String, currentPath:List[String], acc: Int): Int =
      if (fromNode == END_NODE)
        acc + 1
      else
        val nextConnection = getNextConnectionsFor(connectionPairs, fromNode)
        nextConnection.foldLeft(acc) { (acc2, p) =>
          val node = selectNodeFromConnection(fromNode, p)
          node match {
            case c if isNodeUpperCase(c) => findPaths(connectionPairs, c, fromNode :: currentPath, acc2)
            case c if currentPath.contains(c) => acc2
            case c => findPaths(connectionPairs, c, fromNode :: currentPath, acc2)
          }
        }
    val connections = parseConnection(rawConnections)
    findPaths(connections, START_NODE, List(), 0)

  def calculateNumberOfPathsWithSingleTwice(rawConnections: List[String]) =
    def findPaths(connectionPairs: List[Connection], fromNode:String, currentPath:List[String], acc: Int, repeated: Boolean): Int =
      if (fromNode == END_NODE)
        acc + 1
      else
        val nextConnection = getNextConnectionsFor(connectionPairs, fromNode)
        nextConnection.foldLeft(acc) { (acc2, p) =>
          val node = selectNodeFromConnection(fromNode, p)
          node match {
            case c if isNodeUpperCase(c) => findPaths(connectionPairs, c, fromNode :: currentPath, acc2, repeated)
            case c if !repeated && c != START_NODE && currentPath.contains(c) => findPaths(connectionPairs, c, fromNode :: currentPath, acc2, true)
            case c if currentPath.contains(c) => acc2
            case c => findPaths(connectionPairs, c, fromNode :: currentPath, acc2, repeated)
          }
        }
    val connections = parseConnection(rawConnections)
    findPaths(connections, START_NODE, List(), 0, false)

  private def parseConnection(connections: List[String]) = {
    connections.map { raw =>
      val nodes = raw.split("-")
      (nodes(0), nodes(1))
    }
  }

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
