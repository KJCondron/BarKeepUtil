package com.kjcondron.web

import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.io.OutputStreamWriter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Codec.string2codec

import org.xml.sax.InputSource

import dispatch.Http
import dispatch.enrichFuture
import dispatch.implyRequestHandlerTuple
import dispatch.url

object HTTPWrapper {

private def getSaveName(cacheLoc : String)(url : String ) = { 
    val elems = url.split('/')
    cacheLoc + elems(elems.length-1)
  }
}
  
class HTTPWrapper(
    val cacheLocation : String,
    val urlToName : Option[String => String] = None)
{
  val _urlToName = urlToName.getOrElse( HTTPWrapper.getSaveName(cacheLocation) _ )
  
  def request( address : String ) : InputSource = {
    val fl = new File(_urlToName(address))
    if(fl.exists)
      loadStream(_urlToName(address))
    else {
      val h = getISFromURL(address)
      save(h,address)
    }
  }

  def destrcutor = Http.shutdown
    
  private def inputToFile(is: java.io.InputStream, f: java.io.File) {
	  val in = scala.io.Source.fromInputStream(is)("UTF-8")
	  
	  val out = new BufferedWriter(new OutputStreamWriter(
			  new FileOutputStream(f), "UTF-8"));
      try { in.getLines().foreach(out.write(_)) }
	  finally { out.close }
  }
   
  private def saveStream( name : String, is : InputStream ) = 
  {
     val fl = new File(name)
     inputToFile(is, fl)
  }
  
  private def loadStream( name : String ) = 
  { 
    val is = new InputSource(new FileInputStream(new File(name)))
    is.setEncoding("UTF-8")
    is
  }
  
  private def save( input : InputSource, address : String ) : InputSource = 
  {
    val filename = _urlToName(address)
    
    val inputStream = input.getByteStream()
    saveStream(filename, inputStream) 
    
    loadStream(filename)
  }
  
  private def getISFromURL( address : String ) = {
    val page = url(address)
    val fHTML = Http(page OK ( resp => new InputSource(resp.getResponseBodyAsStream()) ))
	val HTML = fHTML()
	HTML.setEncoding("UTF-8");
    HTML
  }
}