package org.fjn.experiment

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

class Soup(matrix:Array[Array[Char]]) {
  val M =  matrix.size
  val N = matrix.head.size


  def containsWord(word:String)={

    (for{i<- 0 until M
        j<- 0 until N}
    yield{
      val history = new ListBuffer[(Int,Int)]()

      resolve(word.toCharArray,(i,j),history)
      history

    }).filter(h => h.length==word.length)

  }

  def resolve(word:Array[Char],position:(Int,Int),history:ListBuffer[(Int,Int)]):Boolean ={


    val (i,j) = position


    if(history.contains(position) || i<0 || j<0 || j>=N || i>=M) {

      false
    }

    else if(word.length==1 && matrix(i)(j)==word.head)  {
      history +=  position
      true
    }
    else{
      val letter = matrix(i)(j)
      if(letter != word.head)
        false
      else{

         history +=  position

         resolve(word.tail,(i+1,j),history)||
         resolve(word.tail,(i-1,j),history)||
         resolve(word.tail,(i,j+1),history)||
         resolve(word.tail,(i,j-1),history)||
         resolve(word.tail,(i+1,j+1),history)||
         resolve(word.tail,(i-1,j+1),history) ||
         resolve(word.tail,(i-1,j-1),history) ||
         resolve(word.tail,(i+1,j-1),history)



      }

    }
  }


}


object test extends  App{

  val random = new scala.util.Random
  val mm: Array[Array[Char]] =
    (for{
      i <- 0 until 100
  }yield {
      (for(j <- 0 until 100)yield{
        (random.nextInt(256)).toChar
      }).toArray
    }).toArray

  mm(3)(3)='s'
  mm(4)(4)='a'
  mm(5)(5)='l'
  mm(6)(6)='s'
  mm(7)(7)='a'
  mm(7)(8)='p'
  mm(7)(9)='a'
  mm(7)(10)='r'
  mm(7)(11)='a'
  mm(7)(12)='t'
  mm(7)(13)='o'
  mm(7)(14)='d'
  mm(7)(15)='o'
  mm(7)(16)='s'





  val s = new Soup(mm)
  val aa = s.containsWord("salsaparatodos")
  println(aa)

}