package nikbde

object Day4 {

  @main def day4_m(): Unit = {
    val bufferedSource = io.Source.fromResource("day4.txt")
    val arr_chr = bufferedSource.getLines().map(_.toArray).toArray

    val arr_width = arr_chr(0).length
    val arr_height = arr_chr.length

    val goodValues = List("XMAS", "SAMX")

    def checkGoodValues(seq: Seq[Char]): Boolean = goodValues.contains(seq.mkString)

    val rows = for {
      i <- 0 until arr_height
      j <- 0 until arr_width - 3
      if checkGoodValues(arr_chr(i).slice(j, j + 4))
    } yield (i, j)

    val columns = for {
      i <- 0 until arr_height - 3
      j <- 0 until arr_width
      if checkGoodValues(Seq(arr_chr(i)(j), arr_chr(i + 1)(j), arr_chr(i + 2)(j), arr_chr(i + 3)(j)))
    } yield (i, j)

    val diagonals1 = for {
      i <- 0 until arr_height - 3
      j <- 0 until arr_width - 3
      if checkGoodValues(Seq(arr_chr(i)(j), arr_chr(i + 1)(j + 1), arr_chr(i + 2)(j + 2), arr_chr(i + 3)(j + 3)))
    } yield (i, j)

    val diagonals2 = for {
      i <- 0 until arr_height - 3
      j <- 3 until arr_width
      if checkGoodValues(Seq(arr_chr(i)(j), arr_chr(i + 1)(j - 1), arr_chr(i + 2)(j - 2), arr_chr(i + 3)(j - 3)))
    } yield (i, j)

    val resXmas = rows.size + columns.size + diagonals1.size + diagonals2.size

    println(resXmas)
  }

}
