/*package com.kjcondron.barkeeputil

import scala.concurrent.ExecutionContext.Implicits.global
import dispatch._
import org.xml.sax.InputSource
import org.ccil.cowan.tagsoup.jaxp.SAXParserImpl
import org.xml.sax.helpers.DefaultHandler
import org.xml.sax.Attributes


class WikiHandler extends DefaultHandler {

  override def startElement( uri : String, localName : String,
                      name : String, a : Attributes)
  
  override def characters( ch : Array[Char], start : Int, length : Int) : Unit =
    {   
    }
    
    override def endElement( uri : String, localName : String, name : String ) = {
    }
    
}

object WikiLookup {
  
  final val PREFIX = "en.wikipedia.org/w/index.php?title="
  final val SUF = """&action=edit"""
  
  def getResult( cocktail : String ) = {
    
    val page = url(PREFIX+cocktail+SUF)
    println(url.toString)
    val fut = Http(page OK ( resp => new InputSource(resp.getResponseBodyAsStream()) ))
//	val is = hmm.completeOption
	val html = fut()
  }
  
}*/