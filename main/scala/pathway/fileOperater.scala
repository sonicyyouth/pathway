package pathway

import pathway.Gsea._
import java.io.{FileWriter, PrintWriter}

object fileOperater {
  def getGeneLogExp(entrezName: String, expFile: String):Array[Float] = {
    scala.io.Source.fromFile(expFile).getLines().drop(2).withFilter(i => i.split("\t")(0).split("\\|")(1) == entrezName).map(_.split("\t")).flatten.drop(1).map { i => math.log(i.toFloat + 1d).toFloat }.toArray
  }

  def geneCorr(gene:Gene, expFile: String,set:Set[String], outFile: String = "./resources/geneExpTemp.txt",logTransform:Boolean=true): Array[(String,Float)] = {
    //val tggeneexp = targetGeneExp(gene,expFile)
    val logGeneExp = if (logTransform) logExpFile(expFile) else getExp(expFile)
    //logGeneExp.take(5).foreach(i => println(i.length,i))
   // println(gene.exp.length)
    val genecorrel = logGeneExp.map{case(k,v) => (k, pathway.calculation.pearsonCorr(gene.exp, v).toFloat)}.filter { case (k, v) => !v.isNaN && k != gene.Entrez && set.contains(k) } //.sortBy(_._2).reverse
    //println(genecorrel.length)
    //genecorrel.take(10).foreach(println)
    if (outFile != null) {
      val geneLs = genecorrel.map(_._1).toSet
      val GeneExpOut = new PrintWriter(new FileWriter(outFile))
      logGeneExp.filter { i => geneLs.contains(i._1)}.foreach { i => GeneExpOut.println(List(i._1,i._2.mkString("\t")).mkString("\t")) }
      GeneExpOut.close()
    }
    genecorrel.sortBy(_._2).reverse
  }

  def logExpFile(expFile: String, outFile: String = "./resources/geneExpLog.txt",geneName:String = null,set:Set[String] = null):Array[(String,Array[Float])] = {
    val Raw = scala.io.Source.fromFile(expFile).getLines()
    val samples = Raw.next()
    val logRaw = Raw.drop(1).map(_.split("\t")).
        map { i => (i.head.split("\\|")(1), i.tail.map { i => if (i == " ") "0" else i }.
        map { i => math.log(i.toFloat + 1d).toFloat }) }.withFilter(i => calculation.Variance(i._2)>0.01)
    val log = if (geneName != null || set != null)
        logRaw.withFilter(i => i._1 != geneName && set.contains(i._1)).toArray
     else logRaw.toArray
    if (outFile != null){
      val GeneExpOut = new PrintWriter(new FileWriter(outFile))
      GeneExpOut.println(samples)
      log.foreach { i => GeneExpOut.println(List(i._1,i._2.mkString("\t")).mkString("\t")) }
      GeneExpOut.close()
    }
    log
  }
  
  def getExp(expFile:String):Array[(String,Array[Float])] = {
    scala.io.Source.fromFile(expFile).getLines().drop(2)
      .map(_.split("\t")).map { i => (i.head.split("\\|")(1), i.tail.map { i => if (i == " ") "0" else i }.map { i => i.toFloat }) }.toArray
  }
}