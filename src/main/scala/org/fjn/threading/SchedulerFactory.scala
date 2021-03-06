package org.fjn.threading

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import akka.actor.Props

trait Semaphore{
  def waitFor(timeMs:Int):Unit
}

object SchedulerFactory {

  def create[A:Manifest,B:Manifest](action:Function[A,B],numberOfWorkers:Int,numberOfResults:Int): (ActorRef,Semaphore,()=>Seq[B]) ={

    val system = ActorSystem("SoupSystem")

    val lck = new java.lang.Object


    val syncArray = new ArrayBuffer[B] with mutable.SynchronizedBuffer[B]
    val listener: ActorRef =  system.actorOf(Props (classOf[Listener[B]],numberOfResults,lck,syncArray))



    val barrier = new Semaphore {

      def waitFor(timeMs:Int){
        lck.synchronized{
          lck.wait(timeMs)
        }
      }
    }

    (system.actorOf(Props(classOf[Scheduler[A,B]],action,numberOfWorkers,listener)), barrier,()=>syncArray.toSeq)

    }

}

class Scheduler[A:Manifest,B:Manifest](action:(A)=>B,numberOfWorker:Int,listener:ActorRef) extends Actor{


  val mA = implicitly[Manifest[A]]
  val mB = implicitly[Manifest[B]]

  val taskRouter = context.actorOf(
    Props(classOf[Worker[A,B]],action,listener)
      .withRouter(RoundRobinRouter(numberOfWorker)), name = "router")



  def receive={
    case x:A=>
      taskRouter ! x
  }
}