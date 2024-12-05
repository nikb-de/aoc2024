package nikbde

import scala.util.matching.Regex

object Day3 {

  val mulPattern: Regex = """mul\((\d+),(\d+)\)""".r
  val getMuls = (str: String) => mulPattern.findAllIn(str).matchData.foldLeft(0)((acc, vl) => acc + vl.group(1).toInt * vl.group(2).toInt)

  @main def day3_m(): Unit =
    println(day3_1())
    println(day3__3())

  def day3__3(): Int =
    val bufferedSource = io.Source.fromResource("real_inputs/day3.txt")
    // bad because allocate to much in memory
    val finalString = bufferedSource.getLines().mkString("\n")
    val fistVals = finalString.substring(0, finalString.indexOf("don't()"))

    val result =
      finalString.split("don't\\(\\)")
      .filter(_.indexOf("do()") > 0)
      .map(x => x.substring(x.indexOf("do()")))
      .foldLeft(0)( (res, str) => getMuls(str) + res )

    result + getMuls(fistVals)


  private def day3_1(): Int =
    val bufferedSource = io.Source.fromResource("real_inputs/day3.txt")
    val result = bufferedSource.getLines().foldLeft(0) {
      (res, str) =>
        res + getMuls(str)
    }
    result

}

