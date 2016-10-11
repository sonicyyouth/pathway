package pathway

import java.io.{FileWriter, PrintWriter}
import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

object arrayOperater {
  def filterSet[@specialized(Float,Int,Double) T: ClassTag](setFile:Array[Array[T]],indx:Array[Int]):Array[Array[T]] = {
    val setLength = setFile.length
    val indxLength = indx.length
    val result:Array[Array[T]] = Array.ofDim(setLength, indxLength)
    var i = 0
    while (i < setLength){
      var j = 0
      while(j < indxLength){
        result(i)(j) = setFile(i)(indx(j))
        j += 1
      }
      i += 1
    }
    
    result
  }
  def getIndex(targetList:Array[String],refList:Array[String]):Array[Int] = {
    val refMap = refList.zipWithIndex.toMap
    val rs:List[Int] = Nil
    for (i <- targetList.map(refMap.get(_))) i match {
      case Some(x) => x :: rs
      case None =>
    }
    rs.reverse.toArray
  }

  def transpose2DArray[@specialized(Float,Int,Double) T: ClassTag](dv: Array[Array[T]]): Array[Array[T]] = {
    val outLength = dv.length
    val innerLength = dv(0).length
    var i = 0
    var out = Array.ofDim[T](innerLength,outLength)
    while ( i < outLength ){
      var j = 0
      while ( j < innerLength ) {
        out(j)(i) = dv(i)(j)
        j += 1
      }
      i += 1
    }
    out
  }
  def filterSetToMat(corr: Array[(String,Float)], geneSetFile:String,gseaFile:String, downhold:Int = 20,uphold:Int = 600,outFile: String = "./resources/GeneSetTemp.txt", filter: Boolean = true) = {
    val tempout = new PrintWriter(new FileWriter(outFile))
    val gseaout = new PrintWriter(new FileWriter(gseaFile))
    val geneSet = scala.io.Source.fromFile(geneSetFile).getLines.map(_.split("\t")).map(i => (i.head, i.tail)).toArray
   // geneSet.take(10).foreach(println)
    val geneList = corr.map(_._1)
    val geneVal = corr.map(_._2)
    val gnMap = geneList.zipWithIndex.toMap
    val gnlsLength = geneList.length
    //println(geneList.length)

    val rslist = new ListBuffer[(String, Float)]
    //var n = 0
    for ((k, ii) <- geneSet) {
      val setMat = getSetArray(ii,gnMap,gnlsLength)

      val msum = setMat.sum.toInt
      //if (n<100) println(msum)
      //n += 1
      if (msum > downhold && msum < uphold) {
        //println(msum)
        val gseares = pathway.calculation.gsea(setMat, geneVal)
        //if(k == "GO:0009952") println(msum,gseares)
        gseaout.println(k + "\t" + gseares._2.mkString("\t"))
        tempout.println(k + "\t" + ii.mkString("\t"))
        rslist += (k -> gseares._1)
      }
    }
    tempout.close
    gseaout.close
    rslist.toArray

  }
  def getSetArray(testSet:Array[String],nmap:Map[String, Int],size:Int):Array[Float] = {
    val setMat = Array.fill[Float](size)(0f)
    for (i <- testSet.map(nmap.get(_))) i match {
      case Some(x) => setMat(x) = 1f
      case None =>
    }
    setMat
  }
}