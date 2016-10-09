package pathway



object Rcall {
  val R = org.ddahl.rscala.RClient() 
  val n = 8
  val sz = 10
  val Rscript =  s"""
         vs <- rbinom($n,size= $sz,prob=0.4)
         m <- matrix(v,nrow=4)
         """
  
  R eval Rscript       
  val v1 = R.getI1("v")
}