package org.fjn.threading

import akka.actor.{ActorRef, Actor}

/**
 * Created by fran on 08.12.13.
 */

class Worker[A,B](action:(A)=>B,listener:ActorRef) extends Actor{

  def receive={
    case x:A => listener ! action(x)
  }
}
