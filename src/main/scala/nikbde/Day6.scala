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

  def getDirection(ch: Char): Option[Direction] = ch match {
    case '^' => Some(Direction.Up)
    case 'v' => Some(Direction.Down)
    case '<' => Some(Direction.Left)
    case '>' => Some(Direction.Right)
    case _ => None
  }


  case class Field(walls: Set[(Int, Int)], xBorder: Int, yBorder: Int)
  case class CursorPos(x: Int, y: Int, direction: Direction)



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


  def getNextPos(cursorPos: CursorPos): CursorPos = {
    val (x, y) = (cursorPos.x, cursorPos.y)
    cursorPos.direction match {
      case Direction.Up => cursorPos.copy(x = x - 1)
      case Direction.Right => cursorPos.copy(y = y + 1)
      case Direction.Down => cursorPos.copy(x = x + 1)
      case Direction.Left => cursorPos.copy(y = y - 1)
    }
  }

  def rotateDirection(cursorPos: CursorPos, walls: Set[(Int, Int)]): CursorPos = {
    cursorPos.direction match {
      case Direction.Up => cursorPos.copy(direction = Direction.Right)
      case Direction.Right => cursorPos.copy(direction = Direction.Down)
      case Direction.Down => cursorPos.copy(direction = Direction.Left)
      case Direction.Left => cursorPos.copy(direction = Direction.Up)
    }
  }


  def playGame(field: Field,
               visited: Set[(Int, Int)],
               cursorPos: CursorPos): Int = {

    val (x, y) = (cursorPos.x, cursorPos.y)
    if (x < 0 || y < 0 || x > field.xBorder || y > field.yBorder) {
      println(visited.toList.sorted(Ordering[(Int, Int)]))
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
    println(playGame(field, Set[(Int, Int)](), cursorPos))
    println(s"xBorder: ${field.xBorder}, yBorder: ${field.yBorder}")
    1
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
    println(counts)
    1
  }

}
