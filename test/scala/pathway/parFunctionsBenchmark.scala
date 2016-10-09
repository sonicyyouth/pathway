package pathway
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import pathway._

object parFunctionsBenchmark extends Bench[Double] {

  /* configuration */

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min[Double],
    measurer)
  lazy val measurer = new Measurer.Default
  lazy val reporter = new LoggingReporter[Double]
  lazy val persistor = Persistor.None

  /* inputs */

  val sizes = Gen.range("size")(10000, 200000, 20000)

  val ranges = for {
    size <- sizes
  } yield 0 until size
  
  val testArrayFloat = ranges.map{i => i.map(_.toFloat).toArray}
  val testArrayInt = ranges.map{i => i.toArray}
  val testArrayDouble = ranges.map{i => i.map(_.toDouble).toArray}
  val parTest = new parFunctions
  parTest.threshold = 5000
  /* tests */

  performance of "parFunction" in {
    measure method "mapPar" in {
      using(testArrayFloat) in {
        r => parTest.mapPar(r)(i => i + 1)
      }
    }
        measure method "mapSeq" in {
      using(testArrayFloat) in {
        r => parTest.mapSeq(r)(i => i+1)
      }
    }
    measure method "map" in {
      using(testArrayFloat) in {
        r => r.map(_ + 1)
      }
    }
    measure method "reducePar" in {
      using(testArrayFloat) in {
        r => parTest.reducePar(r)((i,j) => i+j)
      }
    }
    measure method "mapReduce Float" in {
      using(testArrayFloat) in {
        r => parTest.mapReduce(r)(i => i+2f)((i,j) => i+j)
      }
    }
    measure method "mapReduce Int" in {
      using(testArrayInt) in {
        r => parTest.mapReduce(r)(i => i+2)((i,j) => i+j)
      }
    }
  measure method "mapReduce Double" in {
      using(testArrayDouble) in {
        r => parTest.mapReduce(r)(i => i+2d)((i,j) => i+j)
      }
    }
   measure method "mapPlusReduce Float" in {
      using(testArrayFloat) in {
        r => parTest.mapPlusReduce(r)(i => i+2f)
      }
    }

  }
}