package example

import scala.annotation.tailrec

sealed trait List [+A]
case object Nil extends List[Nothing]
case class Const[+A](h:A,t:List[A]) extends List[A]

object List {

  // A* seq [A]
  def length[A](lst:List[A]):Int= lst match {
    case Nil => 0
    case Const(h,t) => 1 + length(t)
  }
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Const(h,t) => h + sum(t)
  }
  def product(ds: List[Double]):Double = ds match {
    case Nil => 1
    case Const(h,t) => h * product(t)
  }
  def apply[A](as: A*) : List[A] ={
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }
  /*
   Tema 2 Coincidencia de patrones
*/
  /*
  Ejercicio 1 ¿Cuál es el resultado de la siguiente expresión match?
   */
  val x = List(4,5,6,7,8) match {
    case Const(x, Const(5, Const(7, _))) => x
    case Nil => 1
    case Const(x, Const(y, Const(6, Const(7, _)))) => x + y
    case Const(h, t) => h + sum(t)
    case _ => 777
  }
  //El valor que tiene x es 9

  /*
  Ejercicio 2. Implementa la función tail que remueva el primer elemento de un lista
   */
  def tail[A](lst:List[A]):List[A]= lst match {
    case Nil => Nil
    case Const(h,t) => t
  }

  /*
  Ejercicio 3. Implementa la función head que devuelva el primer elemento de la lista.
   */
  def head[A](lst:List[A]):A = lst match {
    case Const(h,t) => h
  }
  /*
    Ejercicio 4. Implemente la siguiente función. def and(lst:List[Boolean]):Boolean = ? ? ?
   */
  def and(lst:List[Boolean]):Boolean = lst match {
    case Nil => true
    case Const(h,Nil) => h
    case Const(h,t) => h && and(t)
  }
  /*
  Ejercicio 5. Implemente la siguiente función. def or(lst:List[Boolean]):Boolean = ???
   */
  def or(lst:List[Boolean]):Boolean = lst match {
    case Nil => false
    case Const(h,Nil) => h
    case Const(h,t) => h || or(t)
  }
  /*
  Ejercicio 6. Implemente la siguiente función.
   */
  def max(lst:List[Int]):Int = {
      @tailrec
      def maxr(lst:List[Int], max:Int):Int = lst match {
        case Nil => max
        case Const (h, t) => maxr (t, if (h > max) h else max)
      }
    maxr(tail(lst),head(lst))
  }
  /*
  Ejercicio 7. Implemente la siguiente función.
   */
  def min(lst:List[Int]):Int = {
    @tailrec
    def minr(lst:List[Int], min:Int):Int = lst match {
      case Nil => min
      case Const (h, t) => minr (t, if (h < min) h else min)
    }
    minr(tail(lst),head(lst))
  }
  /*
  Ejercicio 8. Implemente la siguiente función. def minMax(lst:List[Double]):(Double,Double) = ? ? ?
   */
  def minMax(lst:List[Double]):(Double,Double) = {
    def selectionmm(op:Int, n1:Double,n2:Double):Double = op match {
      case 1 => if(n1 > n2) n1 else n2
      case _ => if(n1 < n2) n1 else n2
    }
    @tailrec
    def minMaxR(lst:List[Double],mm:(Double,Double)):(Double,Double) = lst match {
      case Nil => mm
      case Const(h,t) => minMaxR(t, (selectionmm(0,h,mm._1),selectionmm(1,h,mm._2)))
    }
    minMaxR(tail(lst),(head(lst),head(lst)))
  }
  val l1 = List(4,5,6,7,8)
  val ll = List(4L,5L,6L,7L,8L)
  val lmm = List(4.7,5.1,6.2,7.4,8.7)
  //println(l1)
  //println(tail(List(4)))
  //println(head(l1))
  val lb = List(true,false)
  //println(and(lb))
  //println(or(lb))
  //println(max(l1))
  //println(min(ll))
  //println(minMax(lmm))
  //println("el valor de x es : " + x)

}