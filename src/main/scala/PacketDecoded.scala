import scala.collection.immutable.TreeSet

object PacketDecoded {

  trait Tree[A] {
    def addLeaf(value:A): Tree[A] = this match {
      case Branch(l@((head:Branch[A]) :: tail), o) => Branch(head.addLeaf(value) :: tail, o)
      case Branch(l, op) => Branch(Leaf(value) :: l, op)
      case Leaf(_) => Leaf(value)
      case l:Empty[Int] => Leaf(value)
    }
    def addOperation(op: String, newNode: Boolean):Tree[A] = this match {
      case Branch(l@((head:Branch[A]) :: tail), o) if !newNode => Branch(head.addOperation(op, newNode) :: tail, o)
      case Branch(l, o) if newNode => Branch(Branch(List[Branch[A]](), op) :: l, o)
      case c@Leaf(a) => Branch(List(c), op)
      case _:Empty[Int] => Branch(List(), op)
    }
  }
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](branch: List[Tree[A]], op:String) extends Tree[A]
  class Empty[A] extends Tree[A]

  val hexadecimalToBinary = Map(
    "0" -> "0000",
    "1" -> "0001",
    "2" -> "0010",
    "3" -> "0011",
    "4" -> "0100",
    "5" -> "0101",
    "6" -> "0110",
    "7" -> "0111",
    "8" -> "1000",
    "9" -> "1001",
    "A" -> "1010",
    "B" -> "1011",
    "C" -> "1100",
    "D" -> "1101",
    "E" -> "1110",
    "F" -> "1111"
  )

  val operations = Map(
    List("0","0","0") -> "+",
    List("0","0","1") -> "*",
    List("0","1","0") -> "m",
    List("0","1","1") -> "M",
    List("1","0","1") -> ">",
    List("1","1","0") -> "<",
    List("1","1","1") -> "="

  )

  def fromHexToBinary(hexadecimal: String) = hexadecimal.split("").flatMap { c =>
    hexadecimalToBinary(c).split("")
  }.toList

  def getTotalVersion(rawTransmission: String) =

    def calculateTotalVersion(transmission: List[String], acc: Int): Int =
      if (transmission.isEmpty)
        acc
      else
        val (version, rest) = transmission.splitAt(3)
        rest.splitAt(3) match {
          case (_, rest) if rest.forall(_ == "0") => acc
          case (List("1", "0", "0"), rest) =>
            val (r, digits) = calculateDigitsOfLiteral(rest, List[List[String]]())

            calculateTotalVersion(r, acc + Integer.parseInt(version.mkString, 2))
          case (List(_, _, _), lengthType :: rest) if lengthType == "0" =>
            val (subPacketLength, r) = rest.splitAt(15)
            val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
            val (current, next) = r.splitAt(decimalLength)
            val newAcc = calculateTotalVersion(current, Integer.parseInt(version.mkString, 2) + acc)
            if (next.nonEmpty)
              calculateTotalVersion(next, newAcc)
            else
              newAcc
          case (List(_, _, _), lengthType :: rest) if lengthType == "1" =>
            val (subPacketLength, r) = rest.splitAt(11)
            val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
            calculateTotalVersion(r, Integer.parseInt(version.mkString, 2) + acc)
          case _ => acc
        }

    val transmission = fromHexToBinary(rawTransmission)
    calculateTotalVersion(transmission, 0)

  def evaluateExpression(rawTransmission: String) =
    def traverse(transmission: List[String], acc: List[String]): List[String] =
      if (transmission.isEmpty)
        acc
      else
        val (version, rest) = transmission.splitAt(3)
        rest.splitAt(3) match {
          case (_, rest) if rest.forall(_ == "0") => acc
          case (List("1", "0", "0"), rest) =>
            val (r, digits) = calculateDigitsOfLiteral(rest, List[List[String]]())
            traverse(r, digits.map(literal => Integer.parseInt(literal.mkString, 2).toString) ++ acc)
          case (l@List(a, b, c), lengthType :: rest) if lengthType == "0" =>
            val (subPacketLength, r) = rest.splitAt(15)
            val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
            val (current, next) = r.splitAt(decimalLength)
            val newAcc = traverse(current, operations(l)  :: acc)
            if (next.nonEmpty)
              traverse(next, newAcc)
            else
              newAcc
          case (l@List(a, b, c), lengthType :: rest) if lengthType == "1" =>
            val (subPacketLength, r) = rest.splitAt(11)
            val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
            traverse(r, operations(l)  :: acc)
          case _ => acc
        }
    val transmission = fromHexToBinary(rawTransmission)
    val res = traverse(transmission, List())
    System.out.println(res)
    calculateOperationFrom(res, List(), List()).head

  def evaluateExpression2(rawTransmission: String) =
    def traverse(transmission: List[String], acc: Tree[Int], sameNode: Boolean): (Tree[Int], List[String]) =
      System.out.println(acc)
      if (transmission.isEmpty)
        (acc, List())
      else
          val (version, rest) = transmission.splitAt(3)
          rest.splitAt(3) match {
            case (_, rest) if rest.forall(_ == "0") => (acc, rest)
            case (List("1", "0", "0"), rest) =>
              val (r, digits) = calculateDigitsOfLiteral(rest, List[List[String]]())
              traverse(r, digits.foldLeft(acc) { (acc2, n) =>
                acc2.addLeaf(Integer.parseInt(n.mkString, 2))
              }, true)
            case (l@List(a, b, c), lengthType :: rest) if lengthType == "0" =>
              val (subPacketLength, r) = rest.splitAt(15)
              val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
              val (current, next) = r.splitAt(decimalLength)
              val newAcc = traverse(current, acc.addOperation(operations(l), true), true)
              if (next.nonEmpty)
                traverse(next, newAcc._1, sameNode)
              else
                newAcc
            case (l@List(a, b, c), lengthType :: rest) if lengthType == "1" =>
              val (subPacketLength, r) = rest.splitAt(11)
              val decimalLength = Integer.parseInt(subPacketLength.mkString, 2)
              val newAcc = (1 to decimalLength).foldLeft((acc.addOperation(operations(l), true), r)) { (acc, n) =>
                traverse(acc._2, acc._1, true)
              }
              if (newAcc._2.nonEmpty)
                traverse(newAcc._2, newAcc._1, sameNode)
              else
                newAcc

            case r => (acc, r._2)
          }
    val transmission = fromHexToBinary(rawTransmission)
    val res = traverse(transmission, Empty[Int], false)
    System.out.println(res)
    calculateOperationFrom2(res._1)

  def calculateDigitsOfLiteral(r: List[String], digits:List[List[String]]): (List[String], List[List[String]]) =
    val next = r.splitAt(5)
    if (next._1.head == "0")
      (next._2, next._1.tail :: digits)
    else
      calculateDigitsOfLiteral(next._2, next._1 :: digits)


  def calculateOperationFrom(equation: List[String], operandos: List[Long], acc: List[Long]):  List[Long] =
    def execute(operation: String, values: List[Long]):Long = operation match {
      case "+" => values.sum
      case "*" => values.product
      case "m" => values.min
      case "M" => values.max
      case ">" => if (values.head > values.tail.head) 1L else 0L
      case "<" => if (values.head < values.tail.head) 1L else 0L
      case "=" => if (values. head == values.tail.head) 1L else 0L
    }
    equation match {
      case Nil => acc
      case number :: tail if number.forall(_.isDigit) =>
        calculateOperationFrom(tail, number.toLong :: operandos, acc)
      case operation :: Nil =>
        List(execute(operation, operandos))
      case operation :: a :: tail if !operation.forall(_.isDigit) && !a.forall(_.isDigit) =>
        calculateOperationFrom(a :: tail, execute(operation, operandos) :: acc, acc)
      case operation :: tail if !operation.forall(_.isDigit) =>
        calculateOperationFrom(tail, List(), execute(operation, operandos) :: acc)
  }

  def calculateOperationFrom2(equation: Tree[Int]): Long = equation match {
    case Leaf(value) => value
    case Branch(l, "+") => l.map(calculateOperationFrom2).sum
    case Branch(l, "*") => l.map(calculateOperationFrom2).product
    case Branch(l, "m") => l.map(calculateOperationFrom2).min
    case Branch(l, "M") => l.map(calculateOperationFrom2).max
    case Branch(l, ">") => if (calculateOperationFrom2(l.head) < calculateOperationFrom2(l.tail.head)) 1L else 0L
    case Branch(l, "<") =>
      if (calculateOperationFrom2(l.head) > calculateOperationFrom2(l.tail.head)) 1L else 0L
    case Branch(l, "=") => if (calculateOperationFrom2(l.head) == calculateOperationFrom2(l.tail.head)) 1L else 0L
  }
}
