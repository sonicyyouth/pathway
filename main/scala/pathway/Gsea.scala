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
import sys.process._
object Gsea {
  val gseaCommand = "java -Xmx4G xtools.gsea.Gsea -res /Users/liuqun/workspace/myBio/resources/geneExpLog.txt -cls /Users/liuqun/workspace/myBio/classForGsea.cls#Upf1Low_versus_Upf1High -gmx /Users/liuqun/workspace/myBio/c5.all.v5.2.entrez.gmt -collapse false -mode Max_probe -norm meandiv -nperm 1000 -permute phenotype -rnd_type no_balance -scoring_scheme weighted -rpt_label my_analysis -metric Signal2Noise -sort real -order descending -include_only_symbols true -make_sets true -median false -num 100 -plot_top_x 20 -rnd_seed timestamp -save_rnd_lists false -set_max 500 -set_min 15 -zip_report false -out /Users/liuqun/gsea_home/output/oct11 -gui false"
  var settempFile = "./resources/GeneSetTemp.txt"
  var exptempFile = "./resources/geneExpTemp.txt"
  var expFileforR = "./resources/geneExpLog.txt"
  var RimportFile = "./resources/scalaToR.txt"
  var RoutputFile = "./resources/LimmaPermResults.txt"
  var setDescriptFile = "./resources/GOidDescript.txt"
  var outFile = "./results/GSEA_Result.txt"
  var gseaFile:String = "./resources/Gseaout.txt"

  case class gseaResult(setName: String,descript:String = null, catalog:String = null,esScore: Float, nes: Float, pValue: Float)

  case class Gene(Entrez: String, exp: Array[Float])
  

  def getGsea(geneName: String, expFile: String, geneSetFile: String,permN:Int = 1000,inMem:Boolean = true,perm:Boolean= false,ByCorrelation:Boolean = false): Array[gseaResult] = {
    println(Calendar.getInstance.getTime.toString+"  "+"Start calculating")
  
    def getPermRsByLimma(gene:Gene,expFile:String,gsSet:Set[String],permN1:Int= permN) = {
      val expList = fileOperater.logExpFile(expFile = expFile,outFile = expFileforR)

      val fileToR = new PrintWriter(new FileWriter(RimportFile))
      val expFileR = expFileforR.split("/")(2)
      fileToR.println(expFileR)
      fileToR.println(gene.Entrez)
      fileToR.close()
      //"Rscript limma.R".!
      if(perm) {
        Rcall.n = permN
        Rcall.eval()
      }
      val res = scala.io.Source.fromFile(RoutputFile).getLines
      val geneN = res.next().split("\t")
      val limma = geneN zip res.next().split("\t").map(_.toFloat)
      
      val corr = limma.sortBy(_._2).reverse
      val rslst = arrayOperater.filterSetToMat(corr,geneSetFile,gseaFile)
      val testSet = scala.io.Source.fromFile(settempFile).getLines.map(_.split("\t")).map(i => (i.head, i.tail)).toArray  
      val size = corr.length
      val permrs = res.map(_.split("\t").map(_.toFloat)).toArray
      println(permrs.length)
      val permRes:Array[Array[Float]] = {
        Array.range(0, permN).map {
          ii => {
            val pgeneExp = geneN.zip(permrs(ii)).sortBy(_._2).reverse
            getPGseafromSet(testSet,  pgeneExp, size)
          }
        }
      }
          getgseaResult(rslst,permRes)

    }
    def getPermRsByCorr(gene:Gene,expFile:String,gsSet:Set[String]):Array[gseaResult] = {
      val expList = fileOperater.logExpFile(expFile = expFile,geneName = gene.Entrez,set = gsSet)
      val corr = expList.map{case(k,v) => (k, pathway.calculation.pearsonCorr(gene.exp, v).toFloat)}
      val rslst = arrayOperater.filterSetToMat(corr,geneSetFile,gseaFile)
      val testSet = scala.io.Source.fromFile(settempFile).getLines.map(_.split("\t")).map(i => (i.head, i.tail)).toArray  
      val size = corr.length
      
      val permRes:Array[Array[Float]] = if (inMem) {
        Array.range(0, permN).map {
          ii => {
            val pgeneExp = gettargetPermCorrfromList(gene.exp, expList).sortBy(_._2).reverse
            getPGseafromSet(testSet, pgeneExp, size)
          }
        }
      } else {
        Array.range(0, permN).map {
          ii => {
            val pgeneExp = gettargetPermCorrfromFile(gene.exp, exptempFile).sortBy(_._2).reverse
            getPGseafromFile(settempFile, pgeneExp, size)
          }
        }
      }
      println(permRes.length)
      getgseaResult(rslst,permRes)

      }
      def getGODescript() ={
        val goid = new pathway.GOquery
        val goidFresh = Future{
          goid.connect(goid.GOdescriptQuery)
        }
        var goidMap = scala.collection.immutable.Map[String,(String,String)]()  
        goidFresh onComplete {
          case Success(xs) => {
            println(Calendar.getInstance.getTime.toString+"  "+"connected to GeneOntology.org")
            goid.RS2File(xs,setDescriptFile)
            goidMap = scala.io.Source.fromFile(setDescriptFile).getLines().map(_.split("\t")).withFilter(_.length == 3).map(i => i(0) -> (i(1),i(2))).toMap 

//        while(xs.next){
//          map += (xs.getString(1) -> (xs.getString(2),xs.getString(3)))
//        }
          }
          case Failure(e) => {
            println(Calendar.getInstance.getTime.toString+"  "+"can't connect to GeneOntology.org")
          }
        }  
        goidMap
      }
     def gettargetPermCorrfromFile(midrs:Array[Float],file:String = "./resources/geneExpTemp.txt"):Array[(String,Float)] = {
       val midrsRandom = util.Random.shuffle(midrs.toSeq).toArray
       scala.io.Source.fromFile(file).getLines().map(_.split("\t")).map { ii => (ii.head, pathway.calculation.pearsonCorr(midrsRandom, ii.tail.map(_.toFloat)).toFloat) }.toArray
     }
    def gettargetPermCorrfromList(midrs:Array[Float],exp:Array[(String,Array[Float])]):Array[(String,Float)] = {
      val midrsRandom = util.Random.shuffle(midrs.toSeq).toArray
      exp.map { case(k,v) => (k, pathway.calculation.pearsonCorr(midrsRandom, v))}
    }
    def getPGseafromFile(setFile: String = "./resources/GeneSetTemp.txt", pgeneExp: Array[(String, Float)],nmapSize:Int):Array[Float] = {
      val nmap = pgeneExp.map(_._1).zipWithIndex.toMap
      val pExp = pgeneExp.map(_._2)
      val testSet = scala.io.Source.fromFile(setFile).getLines.map(_.split("\t")).map(i => (i.head, i.tail)).toArray
   // val nmapSize = nmap.size
      testSet.par.map{i => pathway.calculation.gseaPerm(arrayOperater.getSetArray(i._2,nmap,nmapSize),pExp)}.toArray
    }
    def getPGseafromSet(testSet:Array[(String,Array[String])], pgeneExp: Array[(String, Float)],nSize:Int):Array[Float] = {
      val nmap = pgeneExp.map(_._1).zipWithIndex.toMap
      val pExp = pgeneExp.map(_._2)
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
    }
    def getgseaResult(rslst:Array[(String,Float)],permRes:Array[Array[Float]]) = {
        val transpermRes:Array[Array[Float]] = arrayOperater.transpose2DArray(permRes)
        val esScore = rslst.map(_._2).toArray
        println(transpermRes.length)
        println(transpermRes(1).length)
        println(esScore.length)
        val pvalueCal:Array[Float] = arrayPvalue(esScore,transpermRes)
        val nesCal:Array[Float] = arrayNes(esScore,transpermRes)
        val res = Array.range(0,rslst.length).map{i =>
          gseaResult(rslst(i)._1,esScore = rslst(i)._2,nes = nesCal(i),pValue = pvalueCal(i))
        }.sortBy(_.pValue)   
        //val finalResult = res.map(i => mapdes(i,gomap))
        res
    }

    val gomap = getGODescript()
 
    val gene = Gene(geneName, fileOperater.getGeneLogExp(geneName, expFile))

    //gene.exp.take(10).foreach(println)
    //val geneSet = 
    val gsSet = scala.io.Source.fromFile(geneSetFile).getLines.map(_.split("\t").tail).toArray.flatten.toSet
    
    val permRes1 = if (ByCorrelation) getPermRsByCorr(gene,expFile,gsSet) else getPermRsByLimma(gene,expFile,gsSet)
    val permRes = permRes1.map(i => mapdes(i,gomap))
    val gseaOut = new PrintWriter(new FileWriter(outFile))

    permRes.map(i => Vector(i.setName.toString, i.descript.toString,i.catalog.toString,i.esScore.toString,i.nes.toString,i.pValue.toString).mkString("\t")).foreach(gseaOut.println(_))
    println("Map size of GO name is "+gomap.size)

    gseaOut.close()
    println(Calendar.getInstance.getTime.toString+"  "+"calculation completed")

    permRes
  }
} 
 



