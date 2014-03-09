package org.fjn.experiment

import akka.actor.{ActorRef}
import scala.collection.mutable.ListBuffer

import org.fjn.threading.{Semaphore, SchedulerFactory}
import scala.collection.mutable


case class CellResolver(i:Int,j:Int,word:String)

case class Result(lst:ListBuffer[(Int,Int)],length:Int)


class SoupWorker(matrix:Array[Array[Char]]){

  val N=matrix.length
  val M= matrix.head.length

  val cache =  new mutable.HashMap[Char,ListBuffer[(Int,Int)]]()
  (for{i <- 0 until N
                  j <- 0 until M} yield{
    var char = matrix(i)(j)
    if(!cache.contains(char))
      cache(char) = new ListBuffer[(Int,Int)]

     cache(char) += ((i,j))

  })

  def solve(msg:CellResolver): ListBuffer[(Int, Int)] ={


    val history = new ListBuffer[(Int,Int)]
    resolve(msg.word.toCharArray,(msg.i,msg.j),history)
    history
  }
  private def resolve(word: Array[Char], position: (Int, Int), history: ListBuffer[(Int, Int)]): Boolean = {


    val (i, j) = position


    if (history.contains(position) || i < 0 || j < 0 || j >= M || i >= N) {

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



class SoupLetterSolver extends AssertionsForJUnit{


  @ Test def checkSoup{
    val N=300
    val M = 300
    val random = new scala.util.Random
    val mm: Array[Array[Char]] =
      (for {
        i <- 0 until N
      } yield {
        (for (j <- 0 until M) yield {
          (random.nextInt(256)).toChar
        }).toArray
      }).toArray




    mm(13)(13) = 's'
    mm(14)(14) = 'a'
    mm(15)(15) = 'l'
    mm(16)(16) = 's'
    mm(17)(17) = 'a'

    mm(1)(1) = 's'
    mm(2)(2) = 'a'
    mm(3)(3) = 'l'
    mm(4)(4) = 's'
    mm(5)(5) = 'a'


    val soup = new SoupWorker(mm)

    val resolver: (ActorRef, Semaphore, () => Seq[ListBuffer[(Int, Int)]]) =  SchedulerFactory.create(soup.solve,4,N*M)
    for { i<- 0 until N
          j <- 0 until M}{
      resolver._1 ! CellResolver(i,j,"salsa")
    }

    resolver._2.waitFor(Int.MaxValue)




    resolver._3().filter(lst => lst.length=="salsa".length).foreach(println(_))

    println("applicaiton exiting")
  }


}
