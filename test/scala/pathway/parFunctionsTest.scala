package pathway

import org.scalatest.FunSuite
import pathway._
class parFunctionsTest extends FunSuite {
  val parTest = new parFunctions
  parTest.threshold = 5000
  val xs = Array(1 to 10000 :_*).map(_.toString).map(_.toFloat)
  val mapRes = Array(1 to 10 :_*).map(_.toString).map(_.toFloat).map(_+0.5)
  val mapReduceRes:Array[Float] = Array(3.0f, 7.0f, 12.0f, 18.0f, 25.0f, 33.0f, 42.0f, 52.0f, 63.0f, 75.0f)

  test("reducePar") {
    assert(parTest.reducePar(xs)((i,j) => i+j) == 50005000f)
  }
   test("mapPar") {
    assert(parTest.mapPar(xs)(_+0.5).take(10).sameElements(mapRes))
  }
  test("mapReduce") {
    assert(parTest.mapReduce(xs)(i => i+2)((i,j) => i+j).take(10).sameElements(mapReduceRes))
  }

  test("mapPlusReduce") {
    assert(parTest.mapPlusReduce(xs)(i => i+2).take(10).sameElements(mapReduceRes))
  }
}