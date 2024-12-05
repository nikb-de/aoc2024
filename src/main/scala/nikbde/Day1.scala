package nikbde

object Day1 {


  @main def day1_m(): Unit = {
    println(task1())
    println(task2())
  }

  private def read_day11(): (List[Int], List[Int]) = {

    val bufferedSource = io.Source.fromResource("real_inputs/day1.txt")

    bufferedSource.getLines()
      .map(_.split("   ").take(2).map(_.toInt).toList)
      .foldLeft((List[Int](), List[Int]()))((acc, x) => (x(0) :: acc._1, x(1) :: acc._2))

  }


  private def task1(): Int = {
    val tuples = read_day11()
    val sorted1 = tuples._1.sorted
    val sorted2 = tuples._2.sorted
    sorted1.zip(sorted2).foldLeft(0)((acc, x) => acc + Math.abs(x._1 - x._2))
  }

  private def task2(): Int = {
    val tuples = read_day11()
    val sort2Map = tuples._2.groupBy(identity).view.mapValues(_.size)
    tuples._1.map(a => a * sort2Map.getOrElse(a, 0)).sum
  }

}
