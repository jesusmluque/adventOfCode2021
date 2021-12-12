object DumboOctopus {

  type Coordinates = (Int, Int)
  type Matrix = Vector[Vector[Int]]

  case class Grid(private val grid: Matrix) {
    def addOneTo(point: Coordinates):(Grid, Set[Coordinates]) =
      def addOneWithFlashes(g: Grid, point: Coordinates, flashes: Set[Coordinates]): (Grid, Set[Coordinates]) =
        val value = g.grid(point._1)(point._2)
        if (value != 9)
          (Grid(g.grid.updated(point._1, g.grid(point._1).updated(point._2, value + 1))), flashes)
        else
          val newGridAfterFlash = (Grid(g.grid.updated(point._1, g.grid(point._1).updated(point._2, 0))), flashes + point)
          getNeighbors(point).foldLeft(newGridAfterFlash) { (acc, p) =>
            addOneWithFlashes(acc._1, p, acc._2)
          }
      addOneWithFlashes(this, point, Set())

    def getValue(point: Coordinates) = grid(point._1)(point._2)
    def setValue(point: Coordinates, value: Int) = Grid(grid.updated(point._1, grid(point._1).updated(point._2, 0)))
    def foldGrid[A](acc: A)(f: (A, (Coordinates, Int)) => A) = grid.zipWithIndex.foldLeft(acc) { (acc, n) =>
      n._1.zipWithIndex.foldLeft(acc)((acc2, p) => f(acc2, ((n._2, p._2), p._1)))
    }
    def getNeighbors(point: Coordinates) =
        List((point._1 + 1, point._2), (point._1 + 1, point._2 + 1), (point._1, point._2 + 1),
          (point._1 - 1, point._2 + 1), (point._1 - 1, point._2), (point._1 - 1, point._2 - 1), (point._1, point._2 - 1), (point._1 + 1, point._2 - 1)).filter { p =>
            p._1 >= 0 && p._2 >= 0 && p._1 < this.grid.length && p._2 < this.grid.head.length
    }
    def allFlashes =
      this.grid.forall { p =>
        p.forall(_ == 0)
      }
  }
  object Grid {
    def apply(raw: List[String]):Grid =
      Grid(raw.zipWithIndex.foldLeft(Vector.fill(raw.length)(Vector.fill(raw.head.length)(0))) { (acc, row) =>
        acc.updated(row._2, row._1.split("").map(_.toInt).toVector)
      })
  }

  def flashesAfter(cycles: Int, raw: List[String]) =
    val finalGrid = (1 to cycles).foldLeft((Grid(raw), 0)) { (acc, c) =>
      val res = round(acc._1)
      (res._1, res._2 + acc._2)
    }
    finalGrid._2

  def round(grid: Grid) =
    val newGrid = grid.foldGrid((grid, Set[Coordinates]())) { (acc, p) =>
      val res = acc._1.addOneTo(p._1)
      (res._1, res._2 ++ acc._2)
    }
    newGrid._2.foldLeft((newGrid._1, 0)) { (acc, n) =>
      (acc._1.setValue(n, 0), acc._2 + 1)
    }

  def allFlashesAtSameTime(raw: List[String]) =
    def allFlashesRec(grid: Grid, acc: Int): Int =
      if (grid.allFlashes)
        acc
      else
        allFlashesRec(round(grid)._1, acc + 1)
    allFlashesRec(Grid(raw), 0)

}
