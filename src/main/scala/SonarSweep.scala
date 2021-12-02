object SonarSweep {

  def timesOfIncreases(depth: List[Int]):Int =
    def recWithAcc(depth: List[Int], last: Int, acc: Int):Int = depth match
      case Nil => acc
      case head :: tail if head > last => recWithAcc(tail, head, acc + 1)
      case head :: tail => recWithAcc(tail, head, acc)
    recWithAcc(depth.tail, depth.head, 0)

  def timesOfIncreasesWithWindow(depth: List[Int]):Int =
    def recWithAcc(depth:List[Int], newDepth:List[Int], lastWindow:Int, currentWindow:List[Int], acc:Int):Int = depth match
      case Nil => acc
      case head :: tail if currentWindow.size == 3 && currentWindow.sum > lastWindow => recWithAcc(newDepth, newDepth, currentWindow.sum, List(), acc + 1)
      case head :: tail if currentWindow.size == 3 => recWithAcc(newDepth, newDepth, currentWindow.sum, List(), acc)
      case head :: tail if currentWindow.size == 0 => recWithAcc(tail, tail, lastWindow, head :: currentWindow, acc)
      case head :: tail => recWithAcc(tail, newDepth, lastWindow, head :: currentWindow, acc)
    recWithAcc(depth, depth, 0, List(), 0)
}
