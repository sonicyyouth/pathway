package pathway

//import javax.management.Query
import java.io._
import java.net.URLEncoder
import org.apache.http.NameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.message.BasicNameValuePair
import org.apache.commons.httpclient.methods.{PostMethod, StringRequestEntity}
import scala.reflect.io.File
import scala.reflect.io.Path.string2path
//import org.apache.http.impl.client.DefaultHttpClient

/**
  * Created by liuqun on 7/14/16.
  */
class Restful {

  var filter: List[String] = List("710","205")
  var filterName:String = "hgnc_id"
  var datasetName:String =  "hsapiens_gene_ensembl"
  var attrib: List[String] = List("ensembl_gene_id","chromosome_name","start_position","end_position","entrezgene","uniprot_swissprot")



 //.replaceAll("""(?m)[\t\n]+""", "")
  var connectTimeout:Int =10000
  var readTimeout:Int =10000
  var requestMethod: String = "POST"
  def sendXml() = {
    var xmlAttrib = attrib.map(i => s"""<Attribute name = "$i" />""" )
    var xmlAttrStr = xmlAttrib.mkString("")
    var filterStr = filter.mkString(",")
    val urlroot = "http://asia.ensembl.org/biomart/martservice/result?query="
    val xmlhead = """<?xml version="1.0" encoding="UTF-8"?> <!DOCTYPE Query>"""
    val xmlQuery = """<Query  virtualSchemaName = "default" formatter = "TSV" header = "0" uniqueRows = "0" count = "" datasetConfigVersion = "0.6" >"""
    val xmlDataset = s"""<Dataset name = "$datasetName" interface = "default" >"""
    val xmlFilter = s"""<Filter name = "$filterName" value = "$filterStr"/>"""
    val xmltail = """</Dataset></Query>"""

    //val xmlAttribute = """<Attribute name = "ensembl_gene_id" /><Attribute name = "description" /><Attribute name = "chromosome_name" /><Attribute name = "start_position" /><Attribute name = "end_position" /><Attribute name = "hgnc_id" /><Attribute name = "hgnc_symbol" /><Attribute name = "entrezgene" />"""

    //val xmlfilt = <Filter name ={filterName} value = {filter.mkString(",")}/>
    //val xmlfile = <?xml version="1.0" encoding="UTF-8"?> <!DOCTYPE Query><Query  virtualSchemaName = "default" formatter = "TSV" header = "0" uniqueRows = "0" count = "" datasetConfigVersion = "0.6" ><Dataset name = {datasetName} interface = "default" >{xmlfilt}{xmlAttrib}</Dataset></Query>
    val xmlfile = xmlQuery+xmlDataset+xmlFilter+xmlAttrStr+xmltail
    val xmlurlraw = xmlhead+xmlfile
    val xmlurl = URLEncoder.encode(xmlhead+xmlfile,"UTF-8")
    var url = urlroot +xmlurlraw
    val post = new PostMethod(urlroot)
    val requestEntity = new StringRequestEntity(xmlurlraw)
    post.setRequestEntity(requestEntity)
    //val httpclient = new DefaultHttpClient()
    //val response = httpclient.execute(post)
    val resultstream = post.getResponseBodyAsStream()
    //val content = io.Source.fromInputStream(resultstream).getLines()
    //println(resultcode)
    if (false) {


      import java.net.{URL, HttpURLConnection}
      val connection = (new URL(url)).openConnection.asInstanceOf[HttpURLConnection]
      connection.setConnectTimeout(connectTimeout)
      connection.setReadTimeout(readTimeout)
      connection.setRequestMethod(requestMethod)
      val inputStream = connection.getInputStream
      val content = scala.io.Source.fromInputStream(inputStream).getLines()
    }
    //if (inputStream != null) inputStream.close


  }



  val myurl = "http://mygene.info/v3/query"
  var field = List("entrezgene","name","symbol","HGNC","pdb")
  var responseCode = 200
  def sendPost() =  {
    val len = filter.length / 500
    var results = new StringBuilder
      var ii = 0
    val tempfile = "templepdb.txt"

    if(!File(tempfile).exists)File(tempfile).createFile()
    val out = new PrintWriter(new FileWriter(tempfile))
    while ( ii <= len){
        val i = ii*500
        println(i)
        val client = new DefaultHttpClient();
        val post = new HttpPost(myurl);
        var urlParameters = new java.util.ArrayList[NameValuePair]();
        //val params = "q=1017,1018&scopes=entrezgene&fields=name,symbol,uniprot,pdb,hgnc,"
        urlParameters.add(new BasicNameValuePair("scopes", filterName));
        urlParameters.add(new BasicNameValuePair("fields", field.mkString((","))));

        if (ii == len){
         val filterSingle = filter.drop(i)
          urlParameters.add(new BasicNameValuePair("q", filterSingle.mkString(",")));

        } else {
         val filterSingle = filter.drop(i).take(500)
          urlParameters.add(new BasicNameValuePair("q", filterSingle.mkString(",")));

        }
        post.setHeader("content-type", "application/x-www-form-urlencoded");

        post.setEntity(new UrlEncodedFormEntity(urlParameters));

        val response = client.execute(post);
        responseCode = response.getStatusLine().getStatusCode()

        System.out.println("\nSending 'POST' request to URL : " + myurl);
        System.out.println("Post parameters : " + post.getEntity());
        System.out.println("Response Code : " +responseCode);

        var rd = response.getEntity().getContent()
        val result = scala.io.Source.fromInputStream(rd).getLines().drop(1).toList.dropRight(1).mkString("\n")
        results ++= result
      if (ii != len) {
        results ++= ","
        out.println(",")
      }

      if (responseCode != 200) {ii -= 1}
        ii += 1
        out.println(result)
      }
      out.close()
      results.toString()
    }

    // add header





//  def hyperlinkSys = {
//    var url = new URL("http://biodb.jp/convert/acc_id/locusview/AB210043");
//    var con:HttpURLConnection = url.openConnection();
//    con.setRequestMethod("GET");
//    con.connect();
//
//    var reader = new BufferedReader(new InputStreamReader(con.getInputStream()));
    //view
    //while ((line = reader.getLines())!= null){
//      System.out.println(line);

//    reader.close();
//    con.disconnect();
//  }
}
