import scala.annotation.tailrec

object SyntaxScore {

  case class Stack[A](private val stack: List[A]) {
    def foldStack[B](acc: B)(f: (B, A) => B) =
      stack.foldLeft(acc) { (acc2, n) =>
        f(acc2, n)
      }
    def push(elem: A):Stack[A] = Stack(elem :: stack)
    def pull() =
      if (stack.isEmpty)
        None
      else
        Some(stack.head, Stack(stack.tail))
  }

  def getErrorScoreIn(instructions: List[String]) =
    instructions.foldLeft(0L) { (acc, rowInstruction) =>
      detectError(rowInstruction)._2 + acc
    }

  def getValidInstructionsScore(instructions: List[String]) =
    val scores = instructions.map { rowInstruction =>
      detectError(rowInstruction)
    }.filter(_._2 == 0).foldLeft(List[Long]()) { (acc, s) =>
        s._1.foldStack(0L) { (acc2, char) => char match {
          case "(" => acc2 * 5L + 1L
          case "[" => acc2 * 5L + 2L
          case "{" => acc2 * 5L + 3L
          case "<" => acc2 * 5L + 4L
        }
      } :: acc
    }
    val scoresSorted = scores.sorted
    scoresSorted(scoresSorted.length / 2)

  private def detectError(instruction: String):(Stack[String], Long) = {
    val score = Map(")" -> 3L, "]" -> 57L, "}" -> 1197L, ">" -> 25137L)
    @tailrec
    def detectErrorWithAcc(instruction: List[String], acc:(Stack[String], Long)):(Stack[String], Long) =
      instruction match {
        case Nil => acc
        case ")" :: tail if acc._1.pull().get._1 == "(" => detectErrorWithAcc(tail, (acc._1.pull().get._2, acc._2))
        case "}" :: tail if acc._1.pull().get._1 == "{" => detectErrorWithAcc(tail, (acc._1.pull().get._2, acc._2))
        case ">" :: tail if acc._1.pull().get._1 == "<" => detectErrorWithAcc(tail, (acc._1.pull().get._2, acc._2))
        case "]" :: tail if acc._1.pull().get._1 == "[" => detectErrorWithAcc(tail, (acc._1.pull().get._2, acc._2))
        case (c@")") :: tail if acc._1.pull().get._1 != "(" => (acc._1.pull().get._2, acc._2 + score(c))
        case (c@"}") :: tail if acc._1.pull().get._1 != "{" => (acc._1.pull().get._2, acc._2 + score(c))
        case (c@">") :: tail if acc._1.pull().get._1 != "<" => (acc._1.pull().get._2, acc._2 + score(c))
        case (c@"]") :: tail if acc._1.pull().get._1 != "[" => (acc._1.pull().get._2, acc._2 + score(c))
        case c :: tail => detectErrorWithAcc(tail, (acc._1.push(c), acc._2))
      }
    detectErrorWithAcc(parseInstruction(instruction), (Stack(List()), 0L))
  }

  private def parseInstruction(n: String) = {
    n.split("").toList
  }

}
