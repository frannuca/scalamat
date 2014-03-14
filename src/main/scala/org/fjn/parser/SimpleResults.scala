package org.fjn.parser

/**
 * Created by fran on 2/14/14.
 */
trait SimpleResults {
  type Input

  trait Result[+T]{
    def next:Input
  }

  case class Success[+T](result:T,next:Input) extends Result[T]
  case class Failure(msg:String,next:Input) extends  Result[Nothing]

}


object XParser extends SimpleResults{
  type Input = String

  def apply(c:Char)(x:String):Result[Char] ={

    if(x == null || x.isEmpty)
      Failure("this string is empty",x)
    else
      x.charAt(0) match {
        case `c` => Success('x',x.substring(1))
        case _=> Failure("this string does not start with x",x)
      }
  }
}


object testParser extends App{

   val a = XParser('y')("xyz")
  println(a)
}