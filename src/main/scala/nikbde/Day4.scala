package nikbde

object Day4 {

  @main def day4_m: Unit =
    println(day4_1())
    println(day4_2())


  def day4_2(): Int =
  {
    val bufferedSource = io.Source.fromResource("real_inputs/day4.txt")
    val charGrid = bufferedSource.getLines().map(_.toArray).toArray

    val vals = List("MMASS", "SSAMM", "MSAMS", "SMASM")

    val allVals = for {
      x <- charGrid.indices
      y <- charGrid(x).indices
      if x + 2 < charGrid.length && y + 2 < charGrid(x).length
      str = charGrid(x)(y) + charGrid(x)(y + 2).toString + charGrid(x + 1)(y + 1).toString + charGrid(x + 2)(y) + charGrid(x + 2)(y + 2)
      if vals.contains(str)
    } yield (x, y)

    allVals.size
  }

  def day4_1(): Int = {
    val bufferedSource = io.Source.fromResource("real_inputs/day4.txt")
    val charGrid: Array[Array[Char]] = bufferedSource.getLines().map(_.toArray).toArray

    val goodValues: Set[String] = Set("XMAS", "SAMX")
    val directions: List[(Int, Int)] = List(
      (0, 1),  // Horizontal
      (1, 0),  // Vertical
      (1, 1),  // Diagonal 1
      (1, -1)  // Diagonal 2
    )

    def isValidCoordinate(x: Int, y: Int, dx: Int, dy: Int): Boolean =
      (0 until 4).forall(step =>
          x + step * dx >= 0 &&
          x + step * dx < charGrid.length &&
          y + step * dy >= 0 &&
          y + step * dy < charGrid(x).length
      )

    def extractSequence(x: Int, y: Int, dx: Int, dy: Int): Seq[Char] =
      (0 until 4).map(step => charGrid(x + step * dx)(y + step * dy))

    val matches = for {
      x <- charGrid.indices
      y <- charGrid(x).indices
      (dx, dy) <- directions
      if isValidCoordinate(x, y, dx, dy) && goodValues.contains(extractSequence(x, y, dx, dy).mkString)
    } yield (x, y)

    matches.size
  }
}
