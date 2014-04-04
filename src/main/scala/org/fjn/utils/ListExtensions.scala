package org.fjn.utils


/**
 * Created by fran on 26.03.2014.
 */
object ListExtensions {
  def unique2[A](ls:List[A])={
    def loop(set:Set[A],ls:List[A]):List[A]=
      ls match{
        case hd :: tail if set contains hd => loop(set,tail)
        case hd :: tail => hd :: loop(set + hd,tail)
        case Nil => Nil
      }

    loop(Set(),ls)
  }

  implicit def list2Unique[A](ls:List[A])=new {def unique = unique2(ls)}
}
