package nikbde

import scala.collection.mutable

object Day11:

  def parseInput(resourcePath: String): List[Long] =
    scala.io.Source.fromResource(resourcePath).getLines().next()
      .split(" ").map(_.toLong).toList

  def generateNextValues(value: Long): List[Long] =
    if value == 0 then
      List(1)
    else
      val strValue = value.toString
      if strValue.length % 2 == 0 then
        val midPoint = strValue.length / 2
        List(strValue.take(midPoint).toLong, strValue.drop(midPoint).toLong)
      else
        List(value * 2024)

  def memoize[K, V](f: K => V): K => V =
    val cache = mutable.Map.empty[K, V]
    key => cache.getOrElseUpdate(key, f(key))

  def simulateNumberGeneration(iterations: Int, inputPath: String): Long =
    val initialCounts = parseInput(inputPath)
      .groupBy(identity).view.mapValues(_.size.toLong).toMap

    val memoizedNextValues = memoize(generateNextValues)

    val finalCounts = (0 until iterations).foldLeft(initialCounts): 
      (currentCounts, iteration) => 
        
        currentCounts.foldLeft(Map.empty[Long, Long]): 
          (accumulatedCounts, entry) =>
            
            val (currentValue, count) = entry
            memoizedNextValues(currentValue).foldLeft(accumulatedCounts):
              (innerAcc, nextValue) =>
                
                innerAcc + (nextValue -> (innerAcc.getOrElse(nextValue, 0L) + count))
    finalCounts.values.sum

  def day11_1(): Long =
    simulateNumberGeneration(iterations = 25, inputPath = "real_inputs/day11.txt")

  def day11_2(): Long =
    simulateNumberGeneration(iterations = 75, inputPath = "real_inputs/day11.txt")

  @main def day11_main(): Unit =
    println(day11_1())
    println(day11_2())

end Day11
