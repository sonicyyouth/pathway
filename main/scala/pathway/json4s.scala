package pathway

import org.json4s._
import org.json4s.JsonDSL.WithDouble._
import org.json4s.jackson.JsonMethods._

class json4s {
  case class downid(ids:List[String])
  var url = "'https://gdc-api.nci.nih.gov/files"
  var httpHeader = "Content-Type: application/json"
  case class queryStr(filter:Filter, format:String = "tsv",fields:String = "file_id,file_name,cases.submitter_id,data_category,data_type,platform",size:String = "1000")
  case class Filter(op:String = "and",contents:List[Contents])
  case class Contents(op:String = "in",content:Content)
  case class Content(field:String,value:List[String])
  lazy val content1:Content = Content(field = "id",value = List("4948","A17N","A9QX","A9QM"))
  lazy val content2:Content = Content(field = "type",value = List("Gene","gene"))
  lazy val content3:Content = Content(field = "type",value = List("Gene"))

  lazy val contents1:Contents = Contents(op = "in",content = content1)
  lazy val contents2:Contents = Contents(op = "in",content = content2)
  lazy val contents3:Contents = Contents(op = "in",content = content3)

  lazy val filter1:Filter = Filter(op = "and", contents = List(contents1,contents2))
  lazy val filter2:Filter = Filter(op = "and", contents = List(contents1,contents3))
  lazy val filter3:Filter = Filter(op = "and", contents = List(contents1))

  lazy val query1:queryStr = queryStr(filter = filter1,format = "tsv", fields = "file_id,file_name",size = "1000")
  lazy val query2:queryStr = queryStr(filter = filter2,format = "tsv", fields = "file_id,file_name",size = "1000")
  lazy val query3:queryStr = queryStr(filter = filter3,format = "tsv", fields = "file_id,file_name",size = "1000")

  def getDownJson(downids : downid):String = {
    val downjson2 =  ("ids" -> downids.ids) 
    val downjson1 =  ("ids" -> downids.ids(0))
    if (downids.ids.length >1) compact(render(downjson2)) else compact(render(downjson1))
  }
  def contentsQuery(c:Contents) = {
    val rs2 =   (("op" -> c.op)~
           ("content" -> (("field" -> c.content.field) ~
                         ("value" -> c.content.value))
          ))
     val rs1 = (("op" -> c.op)~
           ("content" -> (("field" -> c.content.field) ~
                         ("value" -> c.content.value(0)))
          ))
     if (c.content.value.length >1) rs2 else rs1
  }
  def getQueryJson(query : queryStr):String = {
    val downjson2 = 
      (("filters" ->
        ("op" -> query.filter.op) ~
        ("content" -> query.filter.contents.map(contentsQuery(_))
      ))~
      ("format" -> query.format)~
      ("fields" -> query.fields)~
      ("size" -> query.size))
    val downjson1 = 
      (("filters" ->contentsQuery(query.filter.contents(0))
      )~
      ("format" -> query.format)~
      ("fields" -> query.fields)~
      ("size" -> query.size))      
    
    val downjson = if (query.filter.contents.length >1) downjson2 else downjson1
    compact(render(downjson))
  }
  
}