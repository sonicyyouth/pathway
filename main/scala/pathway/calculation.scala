package pathway

import scala.math._
//import myParallel._
import org.apache.commons.math3.stat.inference.TestUtils
import org.apache.commons.math3.distribution._


import scala.annotation.tailrec
import org.scalameter._
object calculation {
  def mySum(inp:Array[Float],len:Int):Float = {
    var i = 0
    var res = 0f
    while (i<len){
      res += inp(i)
      i += 1
    }
    res
  }

  def Variance(xs: Array[Float]):Float = {
    
    val counts = xs.length
    val accu = mySum(xs,counts)
        
    val mea = accu / counts
    var i = 0
    var variance = 0f
    while ( i< counts){
      variance += (xs(i) - mea)*(xs(i) - mea)
      i += 1
    }
    variance/counts
  }
  def pearsonCorrGeneric[@specialized(Float) T](as: Array[T], bs: Array[T])(implicit n: Numeric[T]): Float = {
    import n._

    val countsA = as.length    
    val countsB = bs.length
    if (countsA != countsB)
      sys.error("you fucked up")
    var i = 0
    var accuA = 0f
    var accuB = 0f
    while (i < countsA){
      accuA += as(i).toFloat
      accuB += bs(i).toFloat
      i += 1
    }
   
    val meaA = accuA / countsA
    val meaB = accuB / countsB
    var variA = 0f
    var variB = 0f
    var ncorrelat = 0f
    

    while (i< countsA){
      ncorrelat = times(as(i),bs(i)).toFloat + ncorrelat
      variA = (as(i).toFloat - meaA)*(as(i).toFloat - meaA) + variA
      variB = (bs(i).toFloat - meaB)*(bs(i).toFloat - meaB) + variB
      i += 1
    }

    ((ncorrelat - countsA.toFloat * meaA * meaB)/scala.math.sqrt(variA * variB)).toFloat

  }
  def pearsonCorr(as: Array[Float], bs: Array[Float]): Float = {
    val countsA = as.length    
    val countsB = bs.length
    if (countsA != countsB)
      sys.error("you fucked up")
    val accuA = mySum(as,countsA)
    val accuB = mySum(bs,countsA)
 
   
    val meaA = accuA / countsA
    val meaB = accuB / countsB
    var i  = 0
    var variA = 0f
    var variB = 0f
    var ncorrelat = 0f
    

    while (i < countsA){
      ncorrelat += as(i)*bs(i)
      variA += (as(i) - meaA)*(as(i) - meaA)
      variB += (bs(i) - meaB)*(bs(i) - meaB)
      i += 1
    }

    (ncorrelat - countsA * meaA * meaB)/(scala.math.sqrt(variA * variB).toFloat)

  }
  def gsea(gss: Array[Float], rss: Array[Float]):(Float,Array[Float]) = {
//    val rs = rss.map(abs(_))
    val len = rss.length
    var rs = new Array[Float](len)
    var posd = 0f
    var i:Int = 0
    var ngss = 0f
    var gsssum = 0f
    while( i < len) {
      val temp = if (rss(i) < 0) -rss(i) else rss(i)
      rs(i) = temp
      posd = posd+ gss(i) * temp
      gsssum = gsssum + gss(i)
      ngss = ngss + 1f
      i += 1
    }
    val negd = ngss - gsssum

    var j = 0
    var res = new Array[Float](len)
    var minresult = 0f
    var maxresult = 0f
    var result = 0f
    while( j < len)  {
      result = result + gss(j) * rs(j) /posd  - (1f - gss(j))/negd
      res(j) = result
      minresult = if (minresult < result) minresult else result
      maxresult = if (maxresult > result) maxresult else result
      j += 1
    }
    val reslt = if(scala.math.abs(minresult) > maxresult) minresult else maxresult
    (reslt,res)
  }
  def gseaPerm(gss: Array[Float], rss: Array[Float]):Float = {
//    import n._
    //val rs = rss.map(abs(_))
    val len = rss.length
    var rs = new Array[Float](len)
    var posd = 0f
    var i:Int = 0
    var ngss = 0f
    var gsssum = 0f
    while( i < len) {
      val temp = if (rss(i) < 0) -rss(i) else rss(i)

      rs(i) = temp
      posd = posd+ gss(i) * temp
      gsssum = gsssum + gss(i)
      ngss = ngss + 1f
      i += 1
    }
    val negd = ngss - gsssum

    //@tailrec def gseainner(gss:Array[Float],rss:Array[Float],posdiv:Float,negdiv:Float,sigresult:Float,minresult: Float,maxresult :Float,result: List[Float]):(Float,Float,List[Float]) ={
    i = 0
    var minresult = 0f
    var maxresult = 0f
    var result = 0f
    while( i < len)  {
      result = result + gss(i) * rs(i) /posd  - (1f - gss(i))/negd
      minresult = if (minresult < result) minresult else result
      maxresult = if (maxresult > result) maxresult else result
      i += 1
    }
    val reslt = if(scala.math.abs(minresult) > maxresult) minresult else maxresult
    reslt
  }
  def pvalue[@specialized(Float) T](x:T,dv: Array[T])(implicit n: Numeric[T]): Float = {

    import n._

    if (gteq(x, zero) && dv.count(gteq(_, zero)) > 0) dv.count { i => gteq(i, x) }.toFloat / dv.count(gteq(_, zero)).toFloat
    else if (lteq(x, zero) && dv.count(lteq(_, zero)) > 0) dv.count { i => lteq(i, x) }.toFloat / dv.count(lteq(_, zero)).toFloat
    else 1f
  }
  def csum(end:Int,start:Int):Int = {
    var msum = 0
    var i = start + 1
    while (i <= end){
      msum = msum + i
      i += 1
    }
    msum
  }
    def cprod(end:BigInt,start:BigInt):BigInt = {
    var prod = BigInt(1)
    var i = start + 1
    while (i <= end){
      prod = prod * i
      i += 1
    }
    prod
  }
  def chisqTest(a:Int,b:Int,c:Int,d:Int):Float = {
    val inp:Array[Array[Long]] = Array(Array(a.toLong,b.toLong),Array(c.toLong,d.toLong))
    TestUtils.chiSquareTest(inp).toFloat
  }
  def chisqTestwithCorrect(a1:Int,b1:Int,c1:Int,d1:Int):Float = {
    val a = BigDecimal(a1)
    val c = BigDecimal(c1)
    val b = BigDecimal(b1)
    val d = BigDecimal(d1)
    val n = (a+b+c+d).toDouble
    val nom = BigDecimal(math.pow(math.max(math.abs(a1*d1-b1*c1)-n/2,0),2)) * BigDecimal(n)
    val denom = if ((a+b)*(a+c)*(b+d)*(c+d) == BigDecimal(0)) BigDecimal(0.0000001) else (a+b)*(a+c)*(b+d)*(c+d)
    val chisq = (nom / denom).toDouble
    chisqPvalue(chisq,1)
  }
  def chisqPvalue(chisq:Double,k:Int):Float = {
    val pval = new ChiSquaredDistribution(k.toDouble).cumulativeProbability(chisq).toFloat
    1-pval
  }

  def fisherTest(a:Int,b:Int,c:Int,d:Int):Float = {
    val Array(af,bf,cf,df) = Array(a,b,c,d).sorted.map(BigInt(_))
    val ab = a+b
    val ac = a+c
    val bd = b+d
    val cd = c+d
    val n = ab+cd
    val Array(abf,acf,bdf,cdf) = Array(ab,ac,bd,cd).sorted
    
  
    val logab = cprod(af,0)
    val logca = cprod(abf,bf)
    val logdc = cprod(acf,cf)
    val logbd = cprod(bdf,df)
    val logn = cprod(n,cdf)
    
    (BigDecimal(logdc*logca*logbd *2) / BigDecimal(logn * logab)).toFloat
  }
  
  
    

  def nes[@specialized(Float) T](x:T,dv: Array[T])(implicit n: Numeric[T]): Float = {
    import n._
    //    val dvar = dv.toArray
    if (gteq(x, zero)) x.toFloat * dv.count(gteq(_, zero)).toFloat / dv.filter(gteq(_, zero)).sum.toFloat
    else
      x.toFloat * dv.count(lteq(_, zero)).toFloat / dv.filter(lteq(_, zero)).sum.toFloat
  }
  
}