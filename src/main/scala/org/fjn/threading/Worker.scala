package org.fjn.threading

import akka.actor.{ActorRef, Actor}

/**
 * Created by fran on 08.12.13.
 */

class Worker[A:Manifest,B:Manifest](action:(A)=>B,listener:ActorRef) extends Actor{

  val mA = implicitly[Manifest[A]]
  val mB = implicitly[Manifest[B]]

  def receive={
    case x:A => listener ! action(x)
  }
}
