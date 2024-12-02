package nikbde

object Day2_2 {

  def generateLists[T](list: List[T]): List[List[T]] = {
    list.zipWithIndex.map{ case (_, index) =>
      list.patch(index, Nil, 1)
    }
  }

  def checkRowCorrectness(str: String): Boolean = {
    val rowList = str.split(" ")
      .map(_.toInt)

    val allLists = generateLists(rowList.toList)

    allLists.map {
      x =>
        val rowParse = x.sliding(2)
          .map(x => Tuple2(x(0) < x(1), Math.abs(x(1) - x(0))))
          .foldLeft(Set[Boolean](), true)({
            case (acc, x) => (
              acc._1 + x._1,
              if (acc._2) x._2 >= 1 && x._2 <= 3 else acc._2
            )
          })

        rowParse match {
          case (set, false) => false
          case (s, true) => if (s.size == 1) true else false
        }
    }.reduce(_ || _)
  }

  @main def day2_2calc(): Unit = {
    val bufferedSource = io.Source.fromResource("day2.txt")
    val resSum = bufferedSource.getLines().count(checkRowCorrectness)
    println(resSum)
  }

}
