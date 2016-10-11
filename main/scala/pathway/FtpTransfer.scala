package pathway

import org.apache.commons.net.ftp._
import java.io.InputStream
import java.io.IOException
import java.io.FileOutputStream


class FtpTransfer {
  var site:String = "ftp.ncbi.nlm.nih.gov"
  var usr:String = "anonymous"
  var pwd:String =""
  var remoteFolder = new String
  var localFolder = new String
  var targetFile = new String
  val ftpClient = new FTPClient()

  //var localFile:String = localFolder+remoteFile

  def ftpConnect():FTPClient = {
    try {
        ftpClient.connect(site)
        ftpClient.enterLocalPassiveMode()
        ftpClient.login(usr, pwd)
        ftpClient.setFileType(FTP.BINARY_FILE_TYPE)
        ftpClient.setBufferSize(1024000)
        println(ftpClient.getStatus())
        //println(ftpClient.listNames("/gene/DATA")(1).mkString)

    //val fileType = ftpClient.BINARY_FILE_TYPE
    //设置文件类型（二进制）
    //ftpClient.setFileType(fileType)
      } catch {
        case e:IOException => e.printStackTrace()
        //throw new RuntimeException("FTP客户端出错！", e)

      }
    return ftpClient
  }
  def listFiles():Array[String]={
    val result = ftpClient.listNames(remoteFolder)
    return result
  }

  def ftpDownload(processing: Boolean = true):String = {
    val ftpClient = ftpConnect()
    val remoteFile = remoteFolder + targetFile
    val localFile = localFolder + targetFile
    //ftpClient.getStatus()
    //ftpClient.listNames("/gene/DATA")
    //listFiles()
    //val localFile:String = remoteFile.split("/").last
    if (processing){
      val unzipFile:String = targetFile.split(".")(0)+".txt"
      val gzipFile = new GzFileProcess
      gzipFile.filename = unzipFile
      var inputStream = ftpClient.retrieveFileStream(remoteFile)
      gzipFile.LinesSelector(inputStream)
    } else {

      val fos = new FileOutputStream(targetFile)
      ftpClient.retrieveFile(remoteFile,fos)
      fos.close()
    }

    return localFile
    //ftpClient.getStatus()

  }
  def ftpStream():InputStream = {
    val ftpClient = ftpConnect()
    val remoteFile = remoteFolder + targetFile
    //val localFile = localFolder + targetFile
    //ftpClient.getStatus()
    //ftpClient.listNames("/gene/DATA")
    //listFiles()
    //val localFile:String = remoteFile.split("/").last
    val ftpStream = ftpClient.retrieveFileStream(remoteFile)
    return ftpStream
  }
  def ftpClose() = {
    try {
      ftpClient.disconnect()
    } catch {
      case e:IOException => e.printStackTrace()
      //throw new RuntimeException("关闭FTP连接发生异常！", e)
    }
  }
}


