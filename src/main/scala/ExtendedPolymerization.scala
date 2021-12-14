import java.util.{Calendar, Date}

object ExtendedPolymerization {

  def getDifferenceMostAndLessOccurrencies(template: String, pairInsertionRawRules: List[String], steps: Int) =
    val pairInsertionRules = pairInsertionRawRules.map { rawRule =>
      val rawRuleSplit = rawRule.split(" -> ")
      (rawRuleSplit(0), rawRuleSplit(1))
    }.toMap

    def buildExpansion(firstElement: String, template: List[String], acc: List[String]): List[String] = template match {
      case Nil => acc ++ List(firstElement)
      case nextElement :: tail => buildExpansion(nextElement, tail, acc ++ (firstElement :: pairInsertionRules(List(firstElement, nextElement).mkString) :: Nil))

    }
    val templateByChar = template.split("").toList
    val finalTemplate = (1 to steps).foldLeft(templateByChar) { (acc, step) =>
      buildExpansion(acc.head, acc.tail, List())
    }
    val occ = finalTemplate.foldLeft(Map[String, Long]()) { (acc, n) =>
      if (acc.contains(n))
        acc.updated(n, acc(n) + 1)
      else
        acc.updated(n, 1)
    }
    occ.maxBy(_._2)._2 - occ.minBy(_._2)._2
}
