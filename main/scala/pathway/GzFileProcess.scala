
package pathway

import java.io._
import java.util.zip.GZIPInputStream
//import scala.util.matching.Regex
/**
  * Created by liuqun on 5/7/16.
  */
class GzFileProcess {
  var filename = new String
  def LinesSelector(inputStream: InputStream) = {
    var gzipStream = new GZIPInputStream(inputStream)
    var decoder : Reader  = new InputStreamReader(gzipStream, "UTF-8")
    var reader:BufferedReader = new BufferedReader(decoder)
    println(filename)
    var line:String = ""
    val file:File = new File(filename)
    var target = new BufferedOutputStream( new FileOutputStream(file) )
    var i  = 1
    while (line != null) {
      if (i < 10){
        println(line)
      }
      i += 1
      if (line matches "^9606.*"){
          target.write((line + "\n").toByte)
          //println(line)

      }
      line = reader.readLine()
    }
      target.close()
  }
}
