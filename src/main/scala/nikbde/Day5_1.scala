package nikbde

import scala.collection.immutable.HashMap

object Day5_1 {

  @main def day5_1m(): Unit = {

    val bufferedSource = io.Source.fromResource("day5.txt").getLines()

    val part = bufferedSource.partition(x => x.contains("|"))

    val depL = part._1.toList

    val depMap = depL.foldLeft(new HashMap[Int, Set[Int]]())((acc, x) => {
      val split = x.split("\\|")
      val key = split(0).toInt
      val value = split(1).toInt
      acc + (key -> (acc.getOrElse(key, Set[Int]()) + value))
    })

    val depMapOpp = depL.foldLeft(new HashMap[Int, Set[Int]]())((acc, x) => {
      val split = x.split("\\|")
      val key = split(1).toInt
      val value = split(0).toInt
      acc + (key -> (acc.getOrElse(key, Set[Int]()) + value))
    })

    println(depMapOpp)

    val badVals = part._2
      .filter(_ != "")
      .map(x => x.split(",").map(_.toInt))
      .filter(x => {
        val goodVals = x.foldLeft((true, Set[Int]()))((acc, y) => {
          val deps = depMap.getOrElse(y, Set[Int]())

          deps.intersect(acc._2) match {
            case x if x.isEmpty => (acc._1, acc._2 + y)
            case _ => (false, acc._2)
          }
        })._1
        !goodVals
      })
      .map(x => reorderBadVals(x, depMapOpp))
      .map( x =>
        println(x.mkString(","))
        x
      )
      .map(x => x((x.length - 1) / 2))


    println(badVals.sum)
  }

  def reorderBadVals(badVals: Array[Int], depMap: HashMap[Int, Set[Int]]): Array[Int] = {

    val setBadVals = badVals.toSet
    val filteredMap = depMap.filter(x => badVals.contains(x._1))
      .map(x => (x._1, x._2.intersect(setBadVals)))


    def topologicalSort(bad: Array[Int], depMap: HashMap[Int, Set[Int]], collected: Set[Int]): Array[Int] = {
      val (noDeps, withDeps) = bad.partition(x => filteredMap.getOrElse(x, Set[Int]()).diff(collected).isEmpty)
      if (noDeps.isEmpty) {
        throw new Exception("No topological sort possible")
      }
      if (withDeps.isEmpty) {
        noDeps
      } else {
        noDeps ++ topologicalSort(withDeps, depMap, collected ++ noDeps)
      }
    }

    topologicalSort(badVals, depMap, Set[Int]())
  }

}
