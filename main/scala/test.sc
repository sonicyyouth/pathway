package pathway
import scala.collection.parallel._
import org.scalatest._
object test {
  println("Welcome to the Scala worksheet")
  val test1 = Array(1 to 10 :_*)
  val threshold = 10000
  test1.sum
  test1.max
  test1.min
 // test1.sum
  test1.length
  
  def time[F](f: => F) = {
    val t0 = System.nanoTime
    val ans = f
    printf("Elapsed: %.3f\n",1e-9*(System.nanoTime-t0))
    ans
  }

  def lots[F](n: Int, f: => F): F = if (n <= 1) f else { f; lots(n-1,f) }
  
  var res = new Array[Float](100)
  
//  import org.scalameter._
  
//  val tim =  measure {
//  (0 until 1000000).toArray
//}

//val mem = measure {
//  for (i <- 0 until 1000000) yield i
//}

lots(3,time(for (i <- 0 until 1000000) yield i))
val matrix = Array.ofDim[Int](2,3)
def myadd : Int => Int = { i : Int => i+3}
}