package org.fjn.threading

import akka.actor.Actor
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

/**
 * Created by fran on 08.12.13.
 */
class Listener[R](maxNumberOfResults: Int,signal:AnyRef, syncArray:mutable.ArrayBuffer[R]) extends Actor {

  def receive = {
    case x: R => {
      syncArray += x

          if (syncArray.length >= maxNumberOfResults){
            context.system.shutdown()

              signal.synchronized{
                signal.notifyAll()

            }
          }

      }

    }


}
