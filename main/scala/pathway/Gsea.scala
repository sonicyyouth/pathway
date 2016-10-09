package pathway

import java.io.{FileWriter, PrintWriter}
import myParallel._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.Calendar
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import shapeless.ops.tuple.Length
import scala.reflect.ClassTag

object Gsea {
  var settempFile = "./resources/GeneSetTemp.txt"
  var exptempFile = "./resources/geneExpTemp.txt"

  case class gseaResult(setName: String,descript:String = null, catalog:String = null,esScore: Float, nes: Float, pValue: Float)

  case class Gene(Entrez: String, exp: Array[Float])

  def getGsea(geneName: String, expFile: String, geneSetFile: String,permN:Int = 1000,inMem:Boolean = true): Array[gseaResult] = {
    println(Calendar.getInstance.getTime.toString+"  "+"Start calculating")
    val goid = new pathway.GOquery
    val goidFresh = Future{
      goid.connect(goid.GOdescriptQuery)
    }
    var goidMap = scala.collection.immutable.Map[String,(String,String)]()
    
    


    goidFresh onComplete {
      case Success(xs) => {
        println(Calendar.getInstance.getTime.toString+"  "+"connected to GeneOntology.org")

        goid.RS2File(xs,"./resources/GOidDescript.txt")
        goidMap = scala.io.Source.fromFile("./resources/GOidDescript.txt").getLines().map(_.split("\t")).withFilter(_.length == 3).map(i => i(0) -> (i(1),i(2))).toMap 

//        while(xs.next){
//          map += (xs.getString(1) -> (xs.getString(2),xs.getString(3)))
//        }
      }
      case Failure(e) => {
        println(Calendar.getInstance.getTime.toString+"  "+"can't connect to GeneOntology.org")
      }
    }


    val gene = Gene(geneName, fileOperater.getGeneLogExp(geneName, expFile))
    //gene.exp.take(10).foreach(println)
    //val geneSet = 
    val gsSet = scala.io.Source.fromFile(geneSetFile).getLines.map(_.split("\t").tail).toArray.flatten.toSet

    val corr = fileOperater.geneCorr(gene, expFile,gsSet)
    //corr.take(10).foreach(println)
    val rslst = arrayOperater.filterSetToMat(corr,geneSetFile)
    val testSet = scala.io.Source.fromFile(settempFile).getLines.map(_.split("\t")).map(i => (i.head, i.tail)).toArray
    val expList = scala.io.Source.fromFile(exptempFile).getLines().map(_.split("\t")).map { ii => (ii.head, ii.tail.map(_.toFloat))}.toArray

    
    val size = corr.length
    val permRes:Array[Array[Float]] = if (inMem) {
      Array.range(0, permN).map {
        ii => {
          val pgeneExp = gettargetPermCorrfromList(gene.exp, expList).sortBy(_._2).reverse
          val nmap = pgeneExp.map(_._1).zipWithIndex.toMap
          //if (ii == 10)println(nmap)
          val pExp = pgeneExp.map(_._2)
          getPGseafromSet(testSet, nmap, size, pExp)
        }
      }
    } else {
      Array.range(0, permN).map {
        ii => {
          val pgeneExp = gettargetPermCorrfromFile(gene.exp, exptempFile).sortBy(_._2).reverse
          val nmap = pgeneExp.map(_._1).zipWithIndex.toMap
          //if (ii == 10)println(nmap)
          val pExp = pgeneExp.map(_._2)
          getPGseafromFile(settempFile, nmap, size, pExp)
        }
      }
    }
//    permRes.take(10).foreach(println)
    //println(permRes.length)
    //println(permRes(0).length)

    val transpermRes:Array[Array[Float]] = arrayOperater.transpose2DArray(permRes)
    val esScore = rslst.map(_._2).toArray
    println(transpermRes.length)
    println(transpermRes(0).length)
    println(esScore.length)

   

    val pvalueCal:Array[Float] = arrayPvalue(esScore,transpermRes)
    val nesCal:Array[Float] = arrayNes(esScore,transpermRes)
    val res = Array.range(0,rslst.length).map{i =>
      gseaResult(rslst(i)._1,esScore = rslst(i)._2,nes = nesCal(i),pValue = pvalueCal(i))
    }.sortBy(_.pValue)

    
    println("Map size of GO name is "+goidMap.size)

    val finalResult = res.map(i => mapdes(i,goidMap))
    println(Calendar.getInstance.getTime.toString+"  "+"calculation completed")
    finalResult
  }
  
  def gettargetPermCorrfromFile(midrs:Array[Float],file:String = "./resources/geneExpTemp.txt"):Array[(String,Float)] = {
    val midrsRandom = util.Random.shuffle(midrs.toSeq).toArray
    scala.io.Source.fromFile(file).getLines().map(_.split("\t")).map { ii => (ii.head, pathway.calculation.pearsonCorr(midrsRandom, ii.tail.map(_.toFloat)).toFloat) }.toArray
  }
  def gettargetPermCorrfromList(midrs:Array[Float],exp:Array[(String,Array[Float])]):Array[(String,Float)] = {
    val midrsRandom = util.Random.shuffle(midrs.toSeq).toArray
    exp.map { case(k,v) => (k, pathway.calculation.pearsonCorr(midrsRandom, v))}
  }



  def getPGseafromFile(setFile: String = "./resources/GeneSetTemp.txt", nmap: Map[String, Int],nmapSize:Int, pExp: Array[Float]):Array[Float] = {
    val testSet = scala.io.Source.fromFile(setFile).getLines.map(_.split("\t")).map(i => (i.head, i.tail)).toArray
   // val nmapSize = nmap.size
    testSet.par.map{i => pathway.calculation.gseaPerm(arrayOperater.getSetArray(i._2,nmap,nmapSize),pExp)}.toArray
  }
  def getPGseafromSet(testSet:Array[(String,Array[String])], nmap: Map[String, Int],nSize:Int, pExp: Array[Float]):Array[Float] = {
    testSet.par.map{i => pathway.calculation.gseaPerm(arrayOperater.getSetArray(i._2,nmap,nSize),pExp)}.toArray
  }
  def arrayPvalue(value: Array[Float],inp:Array[Array[Float]]):Array[Float] = { 
    val len = inp.length
    var i = 0
    var out = new Array[Float](len)
    while (i<len) {
      out(i) = calculation.pvalue(value(i),inp(i))
      i += 1
    }
    out
  }
  def arrayNes(value: Array[Float],inp:Array[Array[Float]]):Array[Float] = { 
    val len = inp.length
    var i = 0
    var out = new Array[Float](len)
    while (i<len) {
      out(i) = calculation.nes(value(i),inp(i))
      i += 1
    }
    out
  }
  def mapdes(gs:gseaResult,map:Map[String,(String,String)]):gseaResult = {
    map.get(gs.setName) match{
      case Some(x) =>  gseaResult(setName = gs.setName,descript = x._1,catalog = x._2,esScore = gs.esScore,nes = gs.nes,pValue = gs.pValue)
      case None => gseaResult(setName = gs.setName,descript = "None",catalog = "None",esScore = gs.esScore,nes = gs.nes,pValue = gs.pValue)
    }
  }
  def getSignGO(file:String,outFile:String = "./results/GSEA_Result.txt") = {
    val result = scala.io.Source.fromFile(file).getLines().map(i => i.split("\t")).map{i => gseaResult(setName = i(0),esScore = i(1).toFloat,nes =i(2).toFloat,pValue = i(3).toFloat)}.toList.sortBy(_.pValue)
    val goid = new pathway.GOquery
    val rs = goid.connect(goid.GOdescriptQuery)
    var map = scala.collection.mutable.Map[String,(String,String)]()
    while (rs.next){
      map += rs.getString(1) -> (rs.getString(2),rs.getString(3))
    }
    def mapDes(gs:gseaResult,map:scala.collection.mutable.Map[String,(String,String)]):gseaResult = {
      val (d,c) = map(gs.setName)
      gseaResult(setName = gs.setName,descript =  d,catalog = c,esScore = gs.esScore,nes = gs.nes,pValue = gs.pValue)
    }
    val outFile = "./results/GSEA_Result.txt"
    val gseaOut = new PrintWriter(new FileWriter(outFile))

    result.map(i => mapDes(i,map)).map(i => Vector(i.setName.toString, i.descript.toString,i.catalog.toString,i.esScore.toString,i.nes.toString,i.pValue.toString).mkString("\t")).foreach(gseaOut.println(_))
    gseaOut.close()
  }
}


