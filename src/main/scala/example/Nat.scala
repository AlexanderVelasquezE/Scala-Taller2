package example

/*
Ejercicio 9. Implemente los Nat en Scala. Tenga en cuenta la siguiente iteración.
 */
sealed trait Nat
case object Cero extends Nat
case class Succ(nat:Nat) extends Nat

object Nat {

  /*
  Tema 3 Definicio de tipos algebraicos los Naturales
   */
  /*
  Ejercicio 10. Implemente la función fromNatToInt que toma un número natural Nat y lo
  transforma a su valor Int.
   */
  def fromNatToInt(nat:Nat):Int = nat match {
    case Cero => 0
    case Succ(nat) => 1 + fromNatToInt(nat)
  }
  /*
  Ejercicio 11. Implemente la función fromIntToNat que tomar valores enteros
positivo (inclusive le cero) y produce el correspondiente número natural.
   */
  //Precondicion: integer >=0
  def fromIntToNat(i:Int):Nat = i match {
    case 0 => Cero
    case n => Succ(fromIntToNat(n-1))
  }
}