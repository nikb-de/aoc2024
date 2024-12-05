package nikbde

import scala.util.matching.Regex

object Day3 {
  @main def day3_1(): Unit =
    val bufferedSource = io.Source.fromResource("day3.txt")
    val result = bufferedSource.getLines().foldLeft(0) {
      (res, str) =>
        val pattern: Regex = """mul\((\d+),(\d+)\)""".r
        val getMuls = pattern.findAllIn(str).matchData.foldLeft(0)((acc, vl) => acc + vl.group(1).toInt * vl.group(2).toInt)
        res + getMuls
    }
    println(result)
}
