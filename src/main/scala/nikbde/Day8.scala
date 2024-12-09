package nikbde

object Day8 {


  @main def day8_m(): Unit =
    //364
    //1231
    println(day8_1())
    println(day8_2())


  case class Field(
    antennas:  Map[Char, Set[(Int, Int)]],
    xBorder: Int,
    yBorder: Int)

  private def parseInput(inputStr: String): Field=
    val bufferedReader = io.Source.fromResource(inputStr)
    val lines = bufferedReader.getLines().toArray

    val mp = for {
      line <- lines.indices
      ch <- lines(line).indices
      if lines(line)(ch) != '.'
    }
    yield (lines(line)(ch), (line, ch))

    val ant = mp.groupBy(a => a(0)).view.foldLeft(Map[Char, Set[(Int, Int)]]())((acc, kv) => acc + (kv(0) -> kv(1).map(_(1)).toSet))

    Field(ant, lines.length - 1, lines(0).length - 1)
  end parseInput



  private def day8_1(): Int =
//    val field = parseInput("test_inputs/day8.txt")
    val field = parseInput("real_inputs/day8.txt")
    println(field)
    val specialPoints = field.antennas.flatMap(
      (k, v) =>
        for {
          a <- v
          b <- v
          if a != b
        }
        yield {
          val diffX = b(0) - a(0)
          val diffY = b(1) - a(1)
          List((a(0) - diffX, a(1) - diffY), (b(0) + diffX, b(1) + diffY))
        }
    )

    println(specialPoints.flatten
      .filter(a => a._1 >= 0 && a._2 >= 0 && a._1 <= field.xBorder && a._2 <= field.yBorder)
      .toSet.size)
    1

  end day8_1


  private def day8_2(): Int =
    //    val field = parseInput("test_inputs/day8.txt")
    val field = parseInput("real_inputs/day8.txt")
    println(field)
    val specialPoints = field.antennas.flatMap(
      (k, v) =>
        for {
          a <- v
          b <- v
          if a != b
        }
        yield {
          val diffX = b(0) - a(0)
          val diffY = b(1) - a(1)
          val rangeAX = if diffX > 0 then Range.inclusive(a(0), 0, -diffX) else Range.inclusive(a(0), field.xBorder, -diffX)
          val rangeAY = if diffY > 0 then Range.inclusive(a(1), 0, -diffY) else Range.inclusive(a(1), field.yBorder, -diffY)
          val rangeBX = if diffX > 0 then Range.inclusive(b(0), field.xBorder, diffX) else Range.inclusive(b(0), 0, -diffX)
          val rangeBY = if diffY > 0 then Range.inclusive(b(1), field.yBorder, diffY) else Range.inclusive(b(1), 0, -diffY)

          rangeAX.zip(rangeAY).map(d => (d(0), d(1))).toList ++ rangeBX.zip(rangeBY).map(d => (d(0), d(1))).toList


//          List((a(0) - diffX, a(1) - diffY), (b(0) + diffX, b(1) + diffY))
        }
    )

    println(specialPoints.flatten
      .toSet.size)
    1

  end day8_2






}
