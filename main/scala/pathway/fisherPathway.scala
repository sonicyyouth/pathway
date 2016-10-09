package pathway

class fisherPathway {
  var setFile:String = "../resources/GOGeneList.txt"
//  var setMat:Array[Array[String]]
  var targetList:Array[String] = Array("")
  var refList:Array[String] = scala.io.Source.fromFile(setFile).getLines().map(_.split("\t").tail).flatten.toSet.toArray
   // val refSet = refSetArray.
  //lazy val allLength = interSect(
  val parFunc = new parFunctions
  parFunc.threshold = 10000
  def getSetHint(usedref:Array[String],refSetArray:Array[Array[String]]):Array[Int] = {
    val gnMap = usedref.zipWithIndex.toMap
    val size = usedref.length
    val rsArray = refSetArray.map(arrayOperater.getSetArray(_,gnMap,size))
    val rs = rsArray.map(parFunc.reducePar(_)((i,j) =>i + j)).map(_.toInt)
    rs
  }
  def getChisqPvalue(targetList:Array[String],refList:Array[String],setFile:String) = {
    val refSetArray = scala.io.Source.fromFile(setFile).getLines().map(_.split("\t").tail).toArray
    val refSet = refSetArray.flatten.toSet
    val setLength = refSetArray.length
    val usedref:Array[String] = refList.filter(refSet.contains(_))
    val usedTarget:Array[String] = targetList.filter(refSet.contains(_))
    val refLength = usedref.length
    val targetLength = usedTarget.length

    val hint = getSetHint(usedTarget,refSetArray).zip(getSetHint(usedref,refSetArray))
    hint.map{case (k,v) => calculation.chisqTestwithCorrect(k,targetLength - k,v-k,refLength -v -targetLength + k)}
  }
  def getTargetMat(usedTarget:Array[String],refSetArray:Array[Array[String]]) = {
    val gnMap = usedTarget.zipWithIndex.toMap
    val size = usedTarget.length
    val rs = refSetArray.map(arrayOperater.getSetArray(_,gnMap,size))
    rs
  }
}