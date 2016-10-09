package pathway

import java.io.{FileWriter, PrintWriter}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.util.Random
import java.util.Calendar


//import scala.collection.JavaConversions._
//package
/**
  * Created by liuqun on 7/14/16.
  */
object myTest extends App{
  //  val gogene = scala.io.Source.fromFile("../resources/GOgeneNoIEA.txt").getLines.drop(1)
  //    .map(_.split("\t")).toList.groupBy(_(0))
  //    .map{case(k,v) => (k,v.map(_(4)))}
  //    val GOChild = GOset.getGoSet
  //    val upf1 = new geneGsea
  //    val GoSet = scala.io.Source.fromFile("/Users/liuqun/workspace/test/GOGeneList.txt").getLines().map(i => i.split("\t")).map(i => (i.head, i.tail.toList)).toList
  var expFile = """../resources/LUAD.rnaseqv2__illuminahiseq_rnaseqv2__unc_edu__Level_3__RSEM_genes_normalized__data.data.txt"""
  val geneSetFile = "../resources/GOGeneList.txt"
  var targetGene = "5976"
//  val goid = new pathway.GOquery
//
//  val goidFresh = Future{
//    println(Calendar.getInstance.getTime.toString+"  "+"start connecting")
//    goid.connect(goid.GOdescriptQuery)
//  }

  

  //    upf1.geneSet = GoSet
  //    .map(_.split("\t")).toList
  val gseax = pathway.Gsea.getGsea(targetGene,expFile,geneSetFile,10000)
//  def myGet(map:Map[String,String],key:String) = {
//    var res = map.get(key) match{
//      case Some(x) => x
//      case None => "None None"
//    }
//    res
//  }
  gseax.take(10).foreach(i => println(i.toString))
  val resultFile = "../results/GseaResult.txt"
  val gseaOut = new PrintWriter(new FileWriter(resultFile))
//  goidFresh.onComplete {
//    case Success(id) => {
//      println(Calendar.getInstance.getTime.toString+"  "+"connection completed")
//      goid.RS2File(id,"GOidDescript.txt")
//      val goidMap:Map[String,String] = scala.io.Source.fromFile("GOidDescript.txt").getLines().map(_.split("\t",2)).withFilter(_.length == 2).map(i => i(0) -> i(1)).toMap 
//      val rs = gseax.map(i => Vector(i.setName.toString, myGet(goidMap,i.setName), i.esScore.toString,i.nes.toString,i.pValue.toString).mkString("\t"))
//      rs.take(10).foreach(println)
//      rs.foreach(gseaOut.println(_))
//    }
//    case Failure(e) => {
//      println(Calendar.getInstance.getTime.toString+"  "+"can't connect to GeneOntology.org")
  val rs = gseax.map(i => Vector(i.setName.toString, i.descript,i.catalog,i.esScore.toString,i.nes.toString,i.pValue.toString).mkString("\t"))
  rs.take(10).foreach(println)
  rs.foreach(gseaOut.println(_))
  gseaOut.close()
}

//  println((gounip++goPDB).distinct.length)





  //  val


//  val gochild = FileOper.filetoTuple("GOchild.txt")
//  var golist = new ListBuffer[GO]
//  for (g <- gochild) {
//    val go = new GO
//    go.Goid = g._1
//    go.goname = g._2
//    g._3 match{
//      case "biological_process" => go.GOFunc = "BP"
//      case "cellular_component" => go.GOFunc = "CC"
//      case "molecular_function" => go.GOFunc = "MF"
//      case _ => go.GOFunc = null
//    }
//    go.GOchild = g._4
//  }
//  println(gochild.take(5).mkString("\n"))

  //rs.close()
  //sqltest.conn.close()

//  val file = "LUAD.rnaseqv2__illuminahiseq_rnaseqv2__unc_edu__Level_3__RSEM_genes_normalized__data.data.txt"
//  val outfile = "tcga_Rnaseq_gene.txt"
//  TCGAFile.RNAexpfileprocessing(file,outfile)
//  val jsonprocess = new JsonProcess
//  val tabfile = outfile.split("\\.")(0) + "class.txt"
//  jsonprocess.file = outfile
//  val geneList = jsonprocess.JsontoCCList()
//  FileOper.caseclasstoFile(tabfile,geneList)


  //val jsontest = new JsonProcess
  //jsontest.genes.foreach(println)
  //println(jsontest.genes)

