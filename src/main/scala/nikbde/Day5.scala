package nikbde

import scala.annotation.tailrec

object Day5 {

  @main def day5_m(): Unit = {
    println(task1())
    println(task2())
  }

  private def task1(): Int = {
    val bufferedSource = io.Source.fromResource("day5.txt").getLines()
    val (dependencyLines, otherLines) = bufferedSource.partition(x => x.contains("|"))
    val depL = dependencyLines.toList
    val dependencyMap = parseDependencies(depL)

    val goodVals = otherLines
      .filter(_ != "")
      .map(x => x.split(",").map(_.toInt))
      .filter(isValid(_, dependencyMap))
      .map(x => x((x.length - 1) / 2))

    goodVals.sum
  }

  private def task2(): Int = {
    val bufferedSource = io.Source.fromResource("day5.txt").getLines()
    val (dependencyLines, otherLines) = bufferedSource.partition(x => x.contains("|"))
    val depL = dependencyLines.toList
    val dependencyMap = parseDependencies(depL)
    val reverseDependencyMap = parseDependencies(depL, reverse = true)


    val badVals = otherLines
      .filter(_ != "")
      .map(x => x.split(",").map(_.toInt))
      .filterNot(isValid(_, dependencyMap))
      .map(x => reorderBadVals(x, reverseDependencyMap))
      .map(x => x((x.length - 1) / 2))

    badVals.sum
  }


  private def isValid(values: Array[Int], depMap: Map[Int, Set[Int]]): Boolean = {
    values.foldLeft((true, Set.empty[Int])) { case ((isValid, collected), value) =>
      val deps = depMap.getOrElse(value, Set.empty)
      if (deps.intersect(collected).nonEmpty) (false, collected)
      else (isValid, collected + value)
    }._1
  }

  private def parseDependencies(lines: List[String], reverse: Boolean = false): Map[Int, Set[Int]] = {
    lines.foldLeft(Map.empty[Int, Set[Int]]) { (acc, line) =>
      val Array(key, value) = line.split("\\|").map(_.toInt)
      val (k, v) = if (reverse) (value, key) else (key, value)
      acc.updated(k, acc.getOrElse(k, Set.empty[Int]) + v)
    }
  }

  private def reorderBadVals(badVals: Array[Int], depMap: Map[Int, Set[Int]]): Array[Int] = {

    val setBadVals = badVals.toSet
    val filteredMap = depMap.filter((key, v) => badVals.contains(key))
      .map((k, v) => (k, v.intersect(setBadVals)))

    @tailrec
    def topologicalSort(bad: Array[Int], depMap: Map[Int, Set[Int]], collected: Array[Int]): Array[Int] = {
      val (noDeps, withDeps) = bad.partition(x => filteredMap.getOrElse(x, Set[Int]()).diff(collected.toSet).isEmpty)
      if (noDeps.isEmpty) {
        throw new Exception("No topological sort possible")
      }
      if (withDeps.isEmpty) {
        collected ++ noDeps
      } else {
         topologicalSort(withDeps, depMap, collected ++ noDeps)
      }
    }

    topologicalSort(badVals, depMap, Array[Int]())
  }

}
