package pathway

import org.scalatest.FunSuite
import pathway._

class RcallTest extends FunSuite {
  test("Rengine") {
    val javaVector = "c(1,2,3,4,5)"
    val engine = Rcall.R
    engine.eval("rVector=" + javaVector)
    engine.eval("meanVal=mean(rVector)")
    val mean = engine.evalD0("meanVal")
    assert(mean == 3.0d)
  }
}
