package org.fjn.threading

import scala.util.{Success,Failure}


object scheduler2 {

  val source = scala.io.Source.fromFile("myText.txt")
  source.toSeq.indexOfSlice("myKeyword")

  import scala.concurrent._
  import ExecutionContext.Implicits.global


  def ![B](onSuccessCallback:Option[B=>Unit]=None, onFailureCallback:Option[Throwable=>Unit]=None)(f:Unit=>B):Future[B]={
       val r: Future[B] = future{
          f()
        }

    r onComplete{
      case Success(x) => onSuccessCallback.map(func => func(x.asInstanceOf[B]))
      case Failure(ex) => onFailureCallback.map(func=>func(ex))
    }

    r
  }
}


