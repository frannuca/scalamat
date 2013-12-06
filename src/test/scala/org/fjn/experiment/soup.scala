package org.fjn.experiment

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContext, Future}

class Soup(matrix: Array[Array[Char]]) {
  val M = matrix.size
  val N = matrix.head.size

  import ExecutionContext.Implicits.global


  def containsWordSingleThreaded(word: String) = {


      (for {i <- 0 until M
            j <- 0 until N}
      yield {


          val history = new ListBuffer[(Int, Int)]()

          resolve(word.toCharArray, (i, j), history)
          history



      }).filter(lst => lst.length == word.length)


  }

  private def solvePartially(n0:Int,n1:Int,word:String): Future[IndexedSeq[ListBuffer[(Int, Int)]]] ={

    scala.concurrent.future {
      (for {i <- n0 until n1
            j <- 0 until M}
      yield {


          val history = new ListBuffer[(Int, Int)]()

          resolve(word.toCharArray, (i, j), history)
          history



      })
    }

  }
    def containsWord(word: String): IndexedSeq[ListBuffer[(Int, Int)]] = {

      var n0 =0
      var n1 = 0
      val split = 4

      (for { i<- 1 to split} yield{
        solvePartially((N/split*(i-1)).toInt,(N/split*i).toInt,word)
      }).flatMap(f => Await.result(f, scala.concurrent.duration.Duration.Inf)).filter(lst => lst.length == word.length)
    }

  def resolve(word: Array[Char], position: (Int, Int), history: ListBuffer[(Int, Int)]): Boolean = {


    val (i, j) = position


    if (history.contains(position) || i < 0 || j < 0 || j >= N || i >= M) {

      false
    }

    else if (word.length == 1 && matrix(i)(j) == word.head) {
      history += position
      true
    }
    else {
      val letter = matrix(i)(j)
      if (letter != word.head)
        false
      else {

        history += position

        resolve(word.tail, (i + 1, j), history) ||
          resolve(word.tail, (i - 1, j), history) ||
          resolve(word.tail, (i, j + 1), history) ||
          resolve(word.tail, (i, j - 1), history) ||
          resolve(word.tail, (i + 1, j + 1), history) ||
          resolve(word.tail, (i - 1, j + 1), history) ||
          resolve(word.tail, (i - 1, j - 1), history) ||
          resolve(word.tail, (i + 1, j - 1), history)


      }

    }
  }


}


object test extends App {

  def time[T](str: String)(thunk: => T): T = {
    print(str + "... ")
    val t1 = System.currentTimeMillis
    val x = thunk
    val t2 = System.currentTimeMillis
    println((t2 - t1) + " msecs")
    x
  }
  val random = new scala.util.Random
  val mm: Array[Array[Char]] =
    (for {
      i <- 0 until 1000
    } yield {
      (for (j <- 0 until 10000) yield {
        (random.nextInt(256)).toChar
      }).toArray
    }).toArray


  mm(113)(113) = 's'
  mm(114)(114) = 'a'
  mm(115)(115) = 'l'
  mm(116)(116) = 's'
  mm(117)(117) = 'a'
  mm(118)(118) = 'p'
  mm(119)(119) = 'a'
  mm(120)(120) = 'r'
  mm(121)(121) = 'a'
  mm(122)(122) = 't'
  mm(123)(123) = 'o'
  mm(124)(124) = 'd'
  mm(125)(125) = 'o'
  mm(126)(126) = 's'

  mm(13)(13) = 's'
  mm(14)(14) = 'a'
  mm(15)(15) = 'l'
  mm(16)(16) = 's'
  mm(17)(17) = 'a'
  mm(17)(18) = 'p'
  mm(17)(19) = 'a'
  mm(17)(20) = 'r'
  mm(17)(21) = 'a'
  mm(17)(22) = 't'
  mm(17)(23) = 'o'
  mm(17)(24) = 'd'
  mm(17)(25) = 'o'
  mm(17)(26) = 's'

  val s = new Soup(mm)
  time("time single threaded"){
  val aa = s.containsWordSingleThreaded("salsaparatodos")
  println(aa.mkString("\n"))
  }

  time("time multi threaded"){
    val aa = s.containsWord("salsaparatodos")
    println(aa.mkString("\n"))
  }

}