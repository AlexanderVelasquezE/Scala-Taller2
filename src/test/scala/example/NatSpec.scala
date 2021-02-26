package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {
  "10.1 La funcion fromNatToInt con Cero " should " 0 " in {
    Nat.fromNatToInt(Cero) shouldBe 0
  }
  "10.2 La funcion fromNatToInt con Succ(Succ(Cero)) " should " 2 " in {
    Nat.fromNatToInt(Succ(Succ(Cero))) shouldBe 2
  }
  "10.3 La funcion fromNatToInt con Succ(Succ(Succ(Succ(Succ(Succ(Succ(Cero))))))) " should " 7 " in {
    Nat.fromNatToInt(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Cero)))))))) shouldBe 7
  }
  "11.1 La funcion fromIntToNat con 0 " should " Cero " in {
    Nat.fromIntToNat(0) shouldBe Cero
  }
  "11.2 La funcion fromIntToNat con 3 " should " Succ(Succ(Succ(Cero))) " in {
    Nat.fromIntToNat(3) shouldBe Succ(Succ(Succ(Cero)))
  }
  "11.3 La funcion fromIntToNat con 7 " should " Succ(Succ(Succ(Succ(Succ(Succ(Succ(Cero))))))) " in {
    Nat.fromIntToNat(7) shouldBe Succ(Succ(Succ(Succ(Succ(Succ(Succ(Cero)))))))
  }


}
