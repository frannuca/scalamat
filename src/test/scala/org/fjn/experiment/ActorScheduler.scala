package org.fjn.experiment

import akka.actor.{ActorRef, ActorSystem, Props, Actor}
import scala.collection.mutable.ListBuffer
import akka.routing.RoundRobinRouter
import akka.pattern.ask
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.collection.immutable.IndexedSeq
import org.fjn.threading.{Semaphore, SchedulerFactory}

/**
 * Created by fran on 07.12.13.
 */

case class CellResolver(i:Int,j:Int,word:String)

case class Result(lst:ListBuffer[(Int,Int)],length:Int)


class SoupWorker(matrix:Array[Array[Char]]){

  val N=matrix.length
  val M= matrix.head.length

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



object SoupLetterSolver extends App{

  val N=3000
  val M = 3000
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