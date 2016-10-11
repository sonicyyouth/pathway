package pathway

import org.scalatest.FunSuite
import pathway._

class jsonTest extends FunSuite {
  

//    val content1:Content = Content(field = "id",value = List("4948","A17N","A9QX","A9QM"))
//    val content2:Content = Content(field = "type",value = List("Gene"))
//    val contents1:Contents = Contents(op = "in",content = content1)
//    val contents2:Contents = Contents(op = "in",content = content2)
//    val filter:Filter = Filter(op = "and", contents = List(contents1,contents2))
//    val query:queryStr = queryStr(filter = filter,format = "tsv", fields = "file_id,file_name",size = "1000")
    val result1 = """{"filters":{"op":"and","content":[{"op":"in","content":{"field":"id","value":["4948","A17N","A9QX","A9QM"]}},{"op":"in","content":{"field":"type","value":["Gene","gene"]}}]},"format":"tsv","fields":"file_id,file_name","size":"1000"}"""
    val result2 = """{"filters":{"op":"and","content":[{"op":"in","content":{"field":"id","value":["4948","A17N","A9QX","A9QM"]}},{"op":"in","content":{"field":"type","value":"Gene"}}]},"format":"tsv","fields":"file_id,file_name","size":"1000"}"""
    val result3 = """{"filters":{"op":"in","content":{"field":"id","value":["4948","A17N","A9QX","A9QM"]}},"format":"tsv","fields":"file_id,file_name","size":"1000"}"""

    val json4sts = new pathway.json4s
    val query1 = json4sts.query1
    val query2 = json4sts.query2
    val query3 = json4sts.query3

  test("json.query") {
    assert(json4sts.getQueryJson(query1) == result1)
  }
 test("json.query with single filter element") {
    assert(json4sts.getQueryJson(query2) == result2)
  }
 test("json.query with single filter") {
    assert(json4sts.getQueryJson(query3) == result3)
  }
    
  }