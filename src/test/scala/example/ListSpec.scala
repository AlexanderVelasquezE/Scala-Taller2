package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec with Matchers {
  "1 El valor de x " should " 9 " in {
    List.x shouldBe 9
  }
  "2.1 La funcion tail con una lista (1,2,3,4,5) " should " (2,3,4,5) " in {
    List.tail(List(1,2,3,4,5)) shouldBe List(2,3,4,5)
  }
  "2.2 La funcion tail con una lista (1) " should " () " in {
    List.tail(List(1)) shouldBe List()
  }
  "3.1 La funcion head con una lista (1,2,3) " should " 1 " in {
    List.head(List(1,2,3)) shouldBe 1
  }
  "3.2 La funcion head con una lista (0) " should " 0 " in {
    List.head(List(0)) shouldBe  0
  }
  "4.1 La funcion and con una lista (true,true,false) " should " false " in {
    List.and(List(true,true,false)) shouldBe false
  }
  "4.2 La funcion and con una lista (true,true,true) " should " true " in {
    List.and(List(true,true,true)) shouldBe true
  }
  "5.1 La funcion or con una lista (false,false,false) " should " false " in {
    List.or(List(false,false,false)) shouldBe false
  }
  "5.2 La funcion or con una lista (false,true,false) " should " true " in {
    List.or(List(false,true,false)) shouldBe true
  }
  "6 La funcion max con una lista (3,1,4,2,5) " should " 5 " in {
    List.max(List( 3,1,4,2,5) ) shouldBe 5
  }
  "7 La funcion min con una lista (3,1,4,2,5) " should " 1 " in {
    List.min(List(3,1,4,2,5) ) shouldBe 1
  }
  "8.1 La funcion minMax con una lista (3,1,4,2,5) " should " (1,5) " in {
    List.minMax(List( 3,1,4,2,5) ) shouldBe (1,5)
  }
  "8.2 La funcion minMax con una lista (3.0,3.1,3.4,3.2,3.5) " should " (3.0,3.5) " in {
    List.minMax(List(3.0,3.1,3.4,3.2,3.5) ) shouldBe (3.0,3.5)
  }


}
