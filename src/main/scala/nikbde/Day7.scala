package nikbde

object Day7 {


  @main def day7M(): Unit =
    println(day7M1())
    println(day7M2())

  def calcResultv1(expectedSum: Long, curSum: Long, leftVals: List[Long]): Boolean =
    if curSum == expectedSum then true
    else if leftVals.isEmpty then false
    else calcResultv1(expectedSum, curSum + leftVals.head, leftVals.tail)
      || calcResultv1(expectedSum, curSum * leftVals.head, leftVals.tail)



  def calcResultv2(expectedSum: Long, curSum: Long, leftVals: List[Long]): Boolean =
    if curSum == expectedSum then true
    else if leftVals.isEmpty then false
    else calcResultv2(expectedSum, curSum + leftVals.head, leftVals.tail)
      || calcResultv2(expectedSum, curSum * leftVals.head, leftVals.tail)
      || calcResultv2(expectedSum, (curSum.toString + leftVals.head.toString).toLong, leftVals.tail)


  private def parseInput(inputStr: String): Map[Long, List[Long]] =
    val bufferedReader = io.Source.fromResource(inputStr)
    val lines = bufferedReader.getLines()
    lines
      .map(_.split(":").map(_.trim))
      .map(arr => (arr(0).toLong, arr(1).split(" ").map(_.toLong).toList)).toMap



  private def day7M1(): Long = {
    val mp = parseInput("real_inputs/day7.txt")
    mp.foldLeft(0L)((acc, kv) => if calcResultv1(kv._1, 0, kv._2.map(_.toLong)) then acc + kv._1 else acc)
  }


  private def day7M2(): Long = {
    val mp = parseInput("real_inputs/day7.txt")
    mp.foldLeft(0L)((acc, kv) => if calcResultv2(kv._1, 0, kv._2.map(_.toLong)) then acc + kv._1 else acc)
  }

}
