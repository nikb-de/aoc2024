package nikbde

object Day4_1 {
  @main def day4_1(): Unit =
  {
    val bufferedSource = io.Source.fromResource("day4.txt")
    val arr_chr = bufferedSource.getLines().map(_.toArray).toArray

    val arr_width = arr_chr(0).length
    val arr_height = arr_chr.length

    val vals = List("MMASS", "SSAMM", "MSAMS", "SMASM")

    val allVals = for {
      i <- 0 until arr_height - 2
      j <- 0 until arr_width - 2
      str = arr_chr(i)(j).toString + arr_chr(i)(j + 2).toString + arr_chr(i + 1)(j + 1).toString + arr_chr(i + 2)(j).toString + arr_chr(i + 2)(j + 2).toString
      if vals.contains(str)
    } yield (i, j)

    val res = allVals.size
    println(res)
  }
}
