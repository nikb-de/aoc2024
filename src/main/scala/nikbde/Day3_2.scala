package nikbde

import scala.util.matching.Regex

object Day3_2 {
  @main def day3__3(): Unit =
    val bufferedSource = io.Source.fromResource("day3.txt")
    // bad because allocate to much in memory
    val finalString = bufferedSource.getLines().mkString("\n")

    val getRes: String => Int = (str: String) =>
      val pattern: Regex = """mul\((\d+),(\d+)\)""".r
      pattern.findAllIn(str).matchData.foldLeft(0)((acc, vl) => acc + vl.group(1).toInt * vl.group(2).toInt)

    val fistVals = finalString.substring(0, finalString.indexOf("don't()"))


    val result =
      finalString.split("don't\\(\\)")
      .filter(_.indexOf("do()") > 0)
      .map(x => x.substring(x.indexOf("do()")))
      .foldLeft(0)(
        (res, str) =>
          getRes(str) + res
      )

    println(result+ getRes(fistVals))
}
