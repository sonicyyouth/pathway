package pathway

object test {
  def testMap(inp:Array[Float]):Array[Float] = {
    val out = inp.map(_.abs)
    out
  }
  def myAbs(x:Float):Float = {
     if (x<0) -x else x
  }
  def testMapmyOp(inp:Array[Float]):Array[Float] = {
    val out = inp.map(myAbs)
    out
  }
  def testMapmyOp2(inp:Array[Float]):Array[Float] = {
    val len = inp.length
    var out = new Array[Float](len)
    var i = 0
    while (i < len){
      out(i) = myAbs(inp(i))
    }
      out
  }
  def mySum[@specialized(Float) T](inp:Array[T],len:Int)(implicit n: Numeric[T]):T = {
    var i = 0
    var res = n.zero
    while (i<len){
      res = n.plus(inp(i),res)
    }
    res
  }
}