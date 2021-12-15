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
    System.out.println(finalTemplate)
    val occ = finalTemplate.foldLeft(Map[String, Long]()) { (acc, n) =>
      if (acc.contains(n))
        acc.updated(n, acc(n) + 1)
      else
        acc.updated(n, 1)
    }
    occ.maxBy(_._2)._2 - occ.minBy(_._2)._2

  def getDifferenceMostAndLessOccurrencies2(template: String, pairInsertionRawRules: List[String], steps: Int) =
    val templateCharList = template.split("").toList

    val pairInsertionRules = pairInsertionRawRules.map { rawRule =>
      val rawRuleSplit = rawRule.split(" -> ")
      (rawRuleSplit(0), rawRuleSplit(1))
    }.toMap

    val templatePairs = templateCharList.tail.foldLeft((templateCharList.head, Map[(String, String), Long](), Map[String, Long](templateCharList.head -> 1L)))  { (acc, c) =>
      (c, acc._2.updated((acc._1, c), acc._2.getOrElse((acc._1, c), 0L) + 1L), acc._3.updated(c, acc._3.getOrElse(c, 0L) + 1L))
    }


    val totalPairs = (1 to steps).foldLeft((templatePairs._2, templatePairs._3)) { (acc, step) =>
      acc._1.foldLeft((Map[(String, String), Long](), acc._2)) { (acc2, oldPair) =>
        val elementToInsert = pairInsertionRules(oldPair._1._1 + oldPair._1._2)

        (acc2._1.updated((oldPair._1._1, elementToInsert), acc2._1.getOrElse((oldPair._1._1, elementToInsert), 0L) + oldPair._2)
          .updated((elementToInsert, oldPair._1._2), acc2._1.getOrElse((elementToInsert, oldPair._1._2), 0L) + oldPair._2), acc._2.updated(elementToInsert, acc._2.getOrElse(elementToInsert, 0L) + oldPair._2))
      }
    }
    System.out.println(totalPairs)

    totalPairs._2.maxBy(_._2)._2 - totalPairs._2.minBy(_._2)._2
}
