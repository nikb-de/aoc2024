package nikbde

import scala.collection.immutable.Queue
import scala.util.chaining.scalaUtilChainingOps

object Day9 {

  @main def day9_m(): Unit =
      println(day9_1("real_inputs/day9.txt"))
      println(day9_2("real_inputs/day9.txt"))

  def day9_1(inputStr: String): Long =
    val bufferedReader = io.Source.fromResource(inputStr)
    val str = bufferedReader.getLines().next().map(x => x.asDigit)
    val (values, emptySpaces) = str.zipWithIndex.partition(x => x(1) % 2 == 0)
    val valsLists = values.zipWithIndex.map(x => List.fill(x(0)(0))(x(1)))
    val flatList = valsLists.flatten
    val queue = Queue(flatList.reverse: _*)


    val finList = emptySpaces.indices.foldLeft((List.empty[Long], queue))(
      (acc, i) => {
        val (res, queue) = acc
        val amountEmpty = emptySpaces(i)(0)
        val (dequeuedElements, remainingQueue) = (1 to amountEmpty).foldLeft((List.empty[Long], queue)) {
          case ((collected, queue), _) =>
            val (element, updatedQueue) = queue.dequeue
            (collected :+ element.toLong, updatedQueue)
        }
        (res ++ valsLists(i).map(_.toLong) ++ dequeuedElements, remainingQueue)
      }
    )

    val res = finList(0).take(flatList.length)
    res.zipWithIndex.map(x => x(0) * x(1)).sum

  end day9_1


  def day9_2(inputStr: String): Long =
    val bufferedReader = io.Source.fromResource(inputStr)
    val str = bufferedReader.getLines().next().map(x => x.asDigit)
    val (values, emptySpaces) = str.zipWithIndex.partition(x => x(1) % 2 == 0)
    val valsVec: Vector[List[Int]] = values.zipWithIndex.map(x => List.fill(x(0)(0))(x(1))).toVector
    val flatList = valsVec.flatten
    val emptyLists = emptySpaces.map(x => List.fill(x(0))(0)).toVector

    val (updatedValsVec, updatedEmptyLists) = valsVec.indices.reverse.foldLeft(valsVec, emptyLists)(
      (acc, inx) => {
        val (curValsVec, curEmptyLists) = acc
        val vec = curValsVec(inx)
        val ind = curEmptyLists.take(inx).indexWhere(x => (x.count(el => el == 0) >= vec.size), 0)
        if ind == -1 then
          (curValsVec, curEmptyLists)
        else
          val elVal = curValsVec(inx).head
          val newValsVecList = List.fill(vec.size)(0)
          val newVec = curValsVec.updated(inx, newValsVecList)
          val emptyL = curEmptyLists(ind)
          val zeroInd = emptyL.indexOf(0)
          val newEmptyList = emptyL.take(zeroInd) ++ List.fill(vec.size)(elVal) ++ emptyL.drop(zeroInd + vec.size)
          (newVec, curEmptyLists.updated(ind, newEmptyList))
      }

    )

    val finList = updatedValsVec.indices.foldLeft(List.empty[Int])(
      (acc, i) => {
        val vec = updatedValsVec(i)
        val emptyList = if i <= updatedEmptyLists.size - 1 then updatedEmptyLists(i) else List[Int]()
        acc ++ vec ++ emptyList
      }
    )
    
    finList.map(x => x.toLong).zipWithIndex.map(x => x(0) * x(1)).sum
    

  end day9_2
  




}
