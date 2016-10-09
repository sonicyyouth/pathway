package pathway



object Rcall {
  val R = org.ddahl.rscala.RClient() 
  val n = 8
  val sz = 10
  var Rscript =  s"""
         source("limma.R")
         x = getPermLimma(permN = 10)
         """
  
       
  def get2DarrayDouble(script:String = Rscript) = {
    R eval script
    R.getD2("x")
  }
  def eval(script:String = Rscript) = {
    R evalD2 script
  }
}