package nikbde

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.parallel.immutable.ParRange

object Day6 {

  @main def day6_m(): Unit =
    println(day6_1())
    println(day6_2())

  enum Direction:
    case Up, Right, Down, Left


  case class Field(
    walls: Set[(Int, Int)],
    xBorder: Int,
    yBorder: Int)


  case class CursorPos(
    x: Int,
    y: Int,
    direction: Direction)

  private def parseInput(input: List[String]): (Field, CursorPos)  =
    val walls = input.zipWithIndex.flatMap {
      case (line, x) => line.zipWithIndex.collect{
      case ('#', y) => (x, y)}
    }.toSet

    val startPos = input.zipWithIndex.flatMap {
      case (line, x) => line.zipWithIndex.collectFirst {
        case (ch, y) if getDirection(ch).isDefined => CursorPos(x, y, getDirection(ch).get)
      }
    }.head

    (Field(walls, input.head.length - 1, input.length - 1), startPos)

  end parseInput


  private def getDirection(ch: Char): Option[Direction] = ch match
    case '^' => Some(Direction.Up)
    case 'v' => Some(Direction.Down)
    case '<' => Some(Direction.Left)
    case '>' => Some(Direction.Right)
    case _ => None


  private def getNextPos(cursorPos: CursorPos): CursorPos =
    cursorPos.direction match
      case Direction.Up => cursorPos.copy(x = cursorPos.x - 1)
      case Direction.Right => cursorPos.copy(y = cursorPos.y + 1)
      case Direction.Down => cursorPos.copy(x = cursorPos.x + 1)
      case Direction.Left => cursorPos.copy(y = cursorPos.y - 1)


  private def rotateDirection(cursorPos: CursorPos, walls: Set[(Int, Int)]): CursorPos =
    cursorPos.direction match
      case Direction.Up => cursorPos.copy(direction = Direction.Right)
      case Direction.Right => cursorPos.copy(direction = Direction.Down)
      case Direction.Down => cursorPos.copy(direction = Direction.Left)
      case Direction.Left => cursorPos.copy(direction = Direction.Up)


  private def isVaildPosition(field: Field, cursorPos: CursorPos): Boolean =
    cursorPos.x >= 0 && cursorPos.y >= 0 &&
      cursorPos.x <= field.xBorder &&
      cursorPos.y <= field.yBorder

//  private def findVisitedTitles(field: Field, cursorPos: CursorPos): Set[(Int, Int)] =

//    @tailrec
//    def traverse(cursorPos: CursorPos,
//                 visited: Set[(Int, Int)]): Set[(Int, Int)] =
//      val curPos = (cursorPos.x, cursorPos.y)
//
//      if (!isVaildPosition(field, cursorPos))
//        then visited
//      else
//        val newVisited = visited + curPos
//        val potenitialPos = getNextPos(cursorPos)





  def getField(): (Field, CursorPos) = {
//    val bufferedSource = io.Source.fromResource("test_inputs/day6_test.txt")
    val bufferedSource = io.Source.fromResource("real_inputs/day6.txt")
    val lines = bufferedSource.getLines().toList
    val walls = scala.collection.mutable.Set[(Int, Int)]()
    var cursorPos: CursorPos = CursorPos(0, 0, Direction.Up)

    for {
      x <- lines.indices
      y <- lines(x).indices
    } {
      val value = lines(x)(y)
      if (value == '#') {
        walls.add((x, y))
      }
      if getDirection(value).isDefined then cursorPos = CursorPos(x, y, getDirection(value).get)
    }

    (Field(walls.toSet, lines(0).length - 1, lines.length - 1), cursorPos)
  }


  def playGame(field: Field,
               visited: Set[(Int, Int)],
               cursorPos: CursorPos): Int = {

    val (x, y) = (cursorPos.x, cursorPos.y)
    if (x < 0 || y < 0 || x > field.xBorder || y > field.yBorder) {
      return visited.size
    }

    val newVisited = visited + ((x, y))

    val potentialPos = getNextPos(cursorPos)
    val nextPos = if field.walls.contains((potentialPos.x, potentialPos.y))
                      then getNextPos(rotateDirection(cursorPos, field.walls))
                    else potentialPos

    playGame(field, newVisited, nextPos)
  }

  @tailrec
  def playGameWithCycles(field: Field,
                         cycleDetector: Set[CursorPos],
                         cursorPos: CursorPos): Boolean = {
    val (x, y) = (cursorPos.x, cursorPos.y)
    if (x < 0 || y < 0 || x > field.xBorder || y > field.yBorder)
      return false

    if (cycleDetector.contains(cursorPos)) {
      return true
    }


    val newCycleDetector = cycleDetector + cursorPos

    val potentialPos = getNextPos(cursorPos)
    val nextPos = if (field.walls.contains((potentialPos.x, potentialPos.y)))
    {
      val rotatePos = rotateDirection(cursorPos, field.walls)
      val potNextPos = getNextPos(rotatePos)
      if !field.walls.contains((potNextPos.x, potNextPos.y)) then potNextPos
      else getNextPos(rotateDirection(rotatePos, field.walls))
    }
      else potentialPos

    playGameWithCycles(field, newCycleDetector, nextPos)
  }


  def day6_1(): Int = {
    val (field, cursorPos) = getField()
    playGame(field, Set[(Int, Int)](), cursorPos)

  }

  def day6_2(): Int = {
    val (field, cursorPos) = getField()
    val addedWall = for {
      x <- ParRange(0, field.xBorder + 1, 1, true)
      y <- ParRange(0, field.yBorder + 1, 1, true)
      if (x, y) != (cursorPos.x, cursorPos.y) && !field.walls.contains((x, y))
    }
      yield
        field.copy(walls = field.walls + ((x, y)))

    val counts = addedWall.count(fl => {
      playGameWithCycles(fl, Set[CursorPos](), cursorPos)
    })
    
    counts
  }

}
