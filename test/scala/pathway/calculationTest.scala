package pathway

import org.scalatest.FunSuite
import pathway._

class calculationTest extends FunSuite {
  test("csum") {
    assert(calculation.csum(100,50) == 3775)
  }
  test("cprod") {
    assert(calculation.cprod(10,5) == 30240)
  }
  test("cprod started with zero") {
    assert(calculation.cprod(10,0) == 3628800)
  }
  test("fisherTest") {
    assert(math.abs(calculation.fisherTest(1,9,11,3) - 0.0026921f) < 0.0000001)
  }
  test("fisherTest with zero") {
    assert(math.abs(calculation.fisherTest(0,10,12,2) - 0.00006730f) < 0.00000001)
  }
  test("fisherTest with big numbers") {
    assert(math.abs(calculation.fisherTest(1,20,12,20000) - 0.0269301f) < 0.0000001)
  }
  test("chisqPvalue") {
    assert(
        math.abs(calculation.chisqPvalue(4.121,1) - 0.04235) < 0.0001)
  }
  test("chisqPvalue can be over 0.5") {
    assert(
        math.abs(calculation.chisqPvalue(0.033822,1) - 0.8541) < 0.0001)
  }
  test("chisqTest with correctness") {
    assert(math.abs(calculation.chisqTestwithCorrect(2,10,12,200) - 0.3579) < 0.0001)
  }
    test("chisqTest with correctness and big number") {
    assert(math.abs(calculation.chisqTestwithCorrect(1,22,162,15969) - 0.5759) < 0.0001)
  }
        test("chisqTest with correctness and zero") {
    assert(math.abs(calculation.chisqTestwithCorrect(233,0,0,15969)) == 0f)
  }
  test("chisqTest without correctness") {
    assert(math.abs(calculation.chisqTest(2,10,12,200) - 0.1254) < 0.0001)
  }

  test("fisherTest with row change") {
    assert(calculation.fisherTest(1,20,12,20000) ==  calculation.fisherTest(20000,20,12,1))
  }

}