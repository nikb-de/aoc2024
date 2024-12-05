package nikbde

import scala.collection.immutable.HashMap

object Day5 {


  @main def day5_m(): Unit = {
    val bufferedSource = io.Source.fromResource("day5.txt").getLines()

    val part = bufferedSource.partition(x => x.contains("|"))

    val depMap = part._1.foldLeft(new HashMap[Int, Set[Int]]())((acc, x) => {
      val split = x.split("\\|")
      val key = split(0).toInt
      val value = split(1).toInt
      acc + (key -> (acc.getOrElse(key, Set[Int]()) + value))
    })

    val sumGoodVals = part._2
      .filter(_ != "")
      .map(x => x.split(",").map(_.toInt))
      .filter(x => {
        x.foldLeft((true, Set[Int]()))((acc, y) => {
          val deps = depMap.getOrElse(y, Set[Int]())

          deps.intersect(acc._2) match {
            case x if x.isEmpty => (acc._1, acc._2 + y)
            case _ => (false, acc._2)
          }
        })._1
      })
      .map(x => x((x.length - 1) / 2))

    println(sumGoodVals.sum)
  }

}
