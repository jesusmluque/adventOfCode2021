object SyntaxScore {

  case class Stack(private val stack: List[String]) {
    def push(elem: String):Stack = Stack(elem :: stack)
    def pull() = (stack.head, Stack(stack.tail))
  }
  object Stack {
    def apply(elements: List[String]):Stack = Stack(elements)
  }

  def getErrorScoreIn(instructions: List[String]) =
    def detectError(instruction: List[String], acc:(Stack, Long)):(Stack, Long) =
      val score = Map(")" -> 3L, "]" -> 57L, "}" -> 1197L, ">" -> 25137L)
      instruction match {
        case Nil => acc
        case ")" :: tail if acc._1.pull()._1 == "(" => detectError(tail, (acc._1.pull()._2, acc._2))
        case "}" :: tail if acc._1.pull()._1 == "{" => detectError(tail, (acc._1.pull()._2, acc._2))
        case ">" :: tail if acc._1.pull()._1 == "<" => detectError(tail, (acc._1.pull()._2, acc._2))
        case "]" :: tail if acc._1.pull()._1 == "[" => detectError(tail, (acc._1.pull()._2, acc._2))
        case (c@")") :: tail if acc._1.pull()._1 != "(" => (acc._1.pull()._2, acc._2 + score(c))
        case (c@"}") :: tail if acc._1.pull()._1 != "{" => (acc._1.pull()._2, acc._2 + score(c))
        case (c@">") :: tail if acc._1.pull()._1 != "<" => (acc._1.pull()._2, acc._2 + score(c))
        case (c@"]") :: tail if acc._1.pull()._1 != "[" => (acc._1.pull()._2, acc._2 + score(c))
    }

    instructions.foldLeft(0L) { (acc, n) =>
      detectError(n.split("").toList, (Stack(List()), 0L))._2 + acc
    }

}
