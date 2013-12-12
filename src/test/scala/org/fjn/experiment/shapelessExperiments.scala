package org.fjn.experiment


import shapeless.poly._
import shapeless._
import syntax.sized._
import poly.identity

object choose extends (Set ~> Option) {
  def apply[T](s : Set[T]) = s.headOption
}
object FoldExamples extends App {


  // Polymorphic binary function value with type-specific cases:
  //  (c : Char, s : String) => s.indexOf(c)
  //  (i : Int, b : Boolean) => if ((i >= 0) == b) "pass" else "fail")
  object combine extends Poly {
    implicit def caseCharString = use((c : Char, s : String) => s.indexOf(c))
    implicit def caseIntBoolean = use((i : Int, b : Boolean) => if ((i >= 0) == b) "pass" else "fail")
  }

//  object addSize extends Poly2 {
//    implicit  def default[T](implicit st: size.Case.Aux[T, Int]) =
//      at[Int, T]{ (acc, t) => acc+size(t) }
//  }
  // Computation is:
  // val c1a = "foo".indexOf('o')
  // val c1b = if ((c1a >= 0) == true) "pass" else "fail"
  val l1 = "foo" :: true :: HNil
  val f1 = l1.foldLeft('o')(combine)
  assert(f1 == "pass")

  // Computation is:
  // val c2a = "bar".indexOf('o')
  // val c2b = if ((c2a >= 0) == false) "pass" else "fail"
  val l2 = "bar" :: false :: HNil
  val f2 = l2.foldLeft('o')(combine)
  assert(f2 == "pass")
}
