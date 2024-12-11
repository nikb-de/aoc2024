package nikbde

object Day10:

  @main
  def day10_m(): Unit =
    println(day10_1())
    println(day10_2())
  
  case class Field(fieldVector: Vector[Vector[Int]], 
                   xBorder: Int, 
                   yBorder: Int)

  def parseInput(inputStr: String): Vector[Vector[Int]] =
    val bufferedReader = io.Source.fromResource(inputStr)
    bufferedReader.getLines().map(x => x.map(_.asDigit).toVector).toVector

  def getNines(field: Field, zeroPos: (Int, Int)): List[(Int, Int)] =
    val xBorder = field.xBorder
    val yBorder = field.yBorder
    val (row, col) = zeroPos

    def getNextEl(curEl: Int, curPos: (Int, Int)): List[(Int, Int)] = 
        def explore(
                     current: (Int, Int),
                     expectedEl: Int,
                     visited: Set[(Int, Int)] = Set.empty
                   ): List[(Int, Int)] = 
          val (x, y) = current
          
          if x < 0 || x >= xBorder || y < 0 || y >= yBorder ||
              expectedEl != field.fieldVector(x)(y) ||
            visited.contains(current) then
            List.empty
            
          else if expectedEl == 9 then
            List(current)
            
          else 
            val directions = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
            directions.flatMap(pos => explore(pos, expectedEl + 1, visited + current))
            
        end explore

        explore(curPos, curEl)
    end getNextEl
     
    getNextEl(0, zeroPos)
  end getNines
  
  def day10_1(): Int =
    val vecInput = parseInput("real_inputs/day10.txt")
    val zeroS = for
      row <- vecInput.indices
      col <- vecInput(row).indices
      if vecInput(row)(col) == 0
    yield (row, col)
    val xBorder = vecInput.length
    val yBorder = vecInput(0).length

    val field = Field(vecInput, xBorder, yBorder)
    val zeroSet = zeroS.flatMap(x => getNines(field, x).distinct)

    zeroSet.size
  end day10_1
  

  def day10_2(): Int =
    val vecInput = parseInput("real_inputs/day10.txt")
    val zeroS = for
      row <- vecInput.indices
      col <- vecInput(row).indices
      if vecInput(row)(col) == 0
    yield (row, col)
    val xBorder = vecInput.length
    val yBorder = vecInput(0).length
    val field = Field(vecInput, xBorder, yBorder)
    val allPaths = zeroS.flatMap(x => getNines(field, x)).size
    allPaths
    
  end day10_2
  
    
end Day10
