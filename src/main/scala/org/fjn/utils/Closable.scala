package org.fjn.utils

object Closable {

  type Disposable = {def close():Unit}

  def using[A, B <: Disposable](closable:B)(f: B =>A):A={
    try{
       f(closable)
    } finally {
      closable.close()
    }
  }
}

object ClosableN {

  type ClosableN = {def close():Unit}

  final class CloseAfter[A<:Product](val x:A){

    def closeAfter[B](f:A=>B):B={
    try{
      f(x)
    } finally{
      for(n <- 0 until x.productArity){
        x.productElement(n) match{
          case c:ClosableN => c.close()
          case _ =>
        }
      }
    }
    }
  }

  implicit def any2Closable[A<:Product](x:A):CloseAfter[A]=
  new CloseAfter[A](x)
}



object  UsageClosable  extends App{

  import Closable._

  import java.io._

  using( new StringReader("fasdfasfasf") ){
    case fr => {
       fr.toString

    }
  }

}
  object UsageCloseableN extends App{

    import ClosableN._
    import java.io._
    (new BufferedReader(new FileReader("in.txt")), new PrintStream(new File("out.txt")), new PrintWriter("sample.txt")).closeAfter{
      case(a,b,c)=> {


      }
    }
 }