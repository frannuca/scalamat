package org.fjn.threading


import akka.actor._
import akka.routing.RoundRobinRouter
import scala.collection.immutable.IndexedSeq
import scala.concurrent.{Await, Future}
import akka.util.Timeout
import scala.concurrent.duration._

sealed trait ScheduleMessage


class Worker[A,B](f:Function1[A,B]) extends Actor {

  def receive = {
    case p =>  sender ! f(p.asInstanceOf[A]) // perform the work
  }
}
class Scheduler[A,B](f:(A)=>B,numberOfWorkers:Int){


  val system = akka.actor.ActorSystem("mainActorSystem")
  val workerRouter = system.actorOf(
    Props(classOf[Worker[A,B]],f).withRouter(RoundRobinRouter(numberOfWorkers)), name = "workerRouter"
  )

  import akka.pattern.ask
  implicit val timeout:Timeout = 10 seconds
  def apply(p:A) ={
    workerRouter ? p
  }

  def stop{
    system.shutdown()
  }
}


object Pi extends App {

  val f = (a:Double) =>{
    (for(i <- 0 until 1000) yield
      math.log(a*a)).sum
  }


  val sch = new Scheduler[Double,Double](f,4)

  val r = for(i <- 0 until 100) yield sch(i.toDouble)
  implicit val timeout = Timeout(5 seconds)
  sch.stop
  r.foreach(x => println(Await.result(x,timeout.duration).asInstanceOf[Double]))




}