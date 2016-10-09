package pathway
import scala.collection.parallel._
import org.scalatest._
import org.scalameter._
import org.ddahl.rscala._

object tests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
   val tim =  measure {
  (0 until 1000000).toArray
}                                                 //> tim  : org.scalameter.Quantity[Double] = 36.440216 ms

val mem = measure {
  for (i <- 0 until 1000000) yield i
}                                                 //> mem  : org.scalameter.Quantity[Double] = 40.317361 ms
val R = org.ddahl.rscala.RClient()                //> java.io.IOException: Cannot run program "R": error=2, No such file or direct
                                                  //| ory
                                                  //| 	at java.lang.ProcessBuilder.start(ProcessBuilder.java:1048)
                                                  //| 	at scala.sys.process.ProcessBuilderImpl$Simple.run(ProcessBuilderImpl.sc
                                                  //| ala:69)
                                                  //| 	at org.ddahl.rscala.RClient$.apply(RClient.scala:704)
                                                  //| 	at org.ddahl.rscala.RClient$.apply(RClient.scala:687)
                                                  //| 	at pathway.tests$$anonfun$main$1.apply$mcV$sp(pathway.tests.scala:16)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at pathway.tests$.main(pathway.tests.scala:7)
                                                  //| 	at pathway.tests.main(pathway.tests.scala)
                                                  //| Caused by: java.io.IOException: error=2, No such file or directory
                                                  //| 	at java.lang.UNIXProcess.forkAndExec(Native M
                                                  //| Output exceeds cutoff limit.

}