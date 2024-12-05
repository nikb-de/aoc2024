package nikbde

object Day2 {

  @main def day2_1(): Unit = {

    val bufferedSource = io.Source.fromResource("day2.txt")
    val resSum = bufferedSource.getLines().foldLeft(0)({
      (row, x) =>
        
      val rowParse = x.split(" ")
          .map(_.toInt)
          .sliding(2)
          .map(x => Tuple2(x(0) < x(1), Math.abs(x(1) - x(0))))
          .foldLeft(Set[Boolean](), true)({
            case (acc, x) => (
              acc._1 + x._1,
              if (acc._2) x._2 >= 1 && x._2 <= 3 else acc._2
            )
          })

      val res = rowParse match {
          case (set, false) => 0
          case (s, true) => if (s.size == 1) 1 else 0
        }

      row + res

    })
    val s =
      """
        |fdafds
        |fdsafdsa
        |fdsafdsa
        |""".stripMargin
    println(resSum)

    println(s)

  }


}
