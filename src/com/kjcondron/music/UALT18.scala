package com.kjcondron.music

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer

import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

import com.kjcondron.web.HTTPWrapper

object UAlt18 extends App {
  
  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\UALT18\""")
    
  def tryGet( a : Attributes, name : String) : Option[String] =
    if(a.getIndex(name) == -1 )
      None
      else
        Some(a.getValue(name))
        
  def tryIf(x : => Boolean) = if(x) Some() else None
        
  def getUALT18Cal( address : String ) : List[String] = 
    getUALT18CalO(address).flatten
  
  
  def getUALT18CalO( address : String ) : List[Option[String]] = {
    
    val HTML = conn.request(address)
		
	val h1 = new UALT18CalHandler
	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
	parser.parse(HTML, h1)
	
	h1.res.toList
	}
  
  def getUALT18( address : String ) : List[Option[String]] = {
	  	
    val HTML = conn.request(address)
	
	val h1 = new UALT18Handler
	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
	parser.parse(HTML, h1)
	
	h1.res.toList.distinct
    }
  
  def getUALT18Res( address : String ) : List[String] = {
    
    val HTML = conn.request(address)
    	
	val h1 = new UALT18ResHandler
	val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
	parser.parse(HTML, h1)
	
	h1.res.toList
}
  
 val address = """http://theunofficialalt18countdownplaylists.com/"""
 val res = getUALT18Cal(address)
 
 val alt18add = res.flatMap( resAdd => getUALT18(resAdd).flatten)
 
 alt18add.foreach( x=> {
   if(x.contains("results-")) {
     println(x)
   }
 })
 
 conn.dispose

class UALT18ResHandler extends DefaultHandler {
    
    var inBody = false
    var foundArtist = false
    var tdCount = 0
    
    val res = ListBuffer[String]()
     
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes) : Unit = {               
      inBody = inBody || name == "body"
        
      tdCount += (if (inBody && foundArtist && name == "td") 1 else 0)
      
    }
                      
    override def characters( ch : Array[Char], start : Int, length : Int) : Unit = {
      if(inBody) {
        foundArtist = foundArtist || ch.contains("ARTIST")
       
        if(tdCount == 5)
        {
          val str = new String(ch, start, length)
          res += str
          tdCount = 0
        }
      }
    }
    
    //override def endElement( uri : String, localName : String, name : String ) =  
  }

  class UALT18CalHandler(val res : ListBuffer[Option[String]] = ListBuffer()) extends DefaultHandler {
    
    var inBody = false
    var inCalendar = false
    var calDepthCount = 0
    var inLink = false
    var foundPrev = true
    
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes) : Unit = {               
      
      inBody = inBody || name == "body"
      
      calDepthCount += (if(inCalendar) 1 else 0)
      
      inCalendar = inCalendar || tryGet(a, "class").map( s => s.contains("calendar") ).getOrElse(false)
      
      if(inCalendar) {
    	  inLink = inLink || (inBody && name == "a")
    	  if(inLink)
    	    res += tryGet(a, "title").flatMap {
    	    case s : String if s.toUpperCase.contains("RESULTS") => Some(a.getValue("href"))
    	    case _ => None
    	  }
    	  
    	  if(foundPrev) {
    	    foundPrev = false
    	    if(inLink)
    	    	res ++= getUALT18CalO( tryGet(a,"href").get )
    	  }
    	  
    	  
    	  foundPrev = foundPrev || tryGet(a, "id").map( _ == "prev" ).getOrElse(false)
      }   
    }
                      
    override def endElement( uri : String, localName : String, name : String ) = { 
      if(inLink) {
        inLink = false
      }
    
    if(inCalendar) {
      calDepthCount -=1
    		  if(calDepthCount == 0) { 
    			  inCalendar = false
      			}
    	}
    }
  }
  
  class UALT18Handler extends DefaultHandler {
    
    var inBody = false
    var inLink = false
    
    val res = ListBuffer[Option[String]]()
    
    override def startElement( uri : String, localName : String,
                      name : String, a : Attributes) : Unit = {               
      
      inBody = inBody || name == "body"
      //inLink = inLink || (inBody && name == "a")  
         
      /*if(inBody && name == "a") {
        res += tryGet(a, "title").flatMap {
          case s : String if s.toUpperCase.contains("RESULTS") => 
            {
              val pattern ="""Results (\d+)/(\d+)/(\d+)""".r
              pattern.findFirstIn(s).flatMap( _ => {
            	  val res = tryGet(a,"href").flatMap( href => {
            		 if(href.toUpperCase().contains("RESULTS"))	  
            		   Some(href)
            		 else
            		   None
            	  })
            	  res
              } )
            }
          case _ => None
        }*/
        
       val pattern ="""Results (\d+)/(\d+)/(\d+)""".r	
        
        if(inBody && name == "a") {
	        res += tryGet(a, "title").flatMap {
			  case s : String => tryIf(s.toUpperCase.contains("RESULTS")).flatMap( _ =>
				  pattern.findFirstIn(s).flatMap( _ => 
					  tryGet(a,"href").flatMap( href => 
						 tryIf(href.toUpperCase().contains("RESULTS")).map(_ => href)	  
					  )
				))
			  case _ => None
			}
        
        val el = List[Option[String]]()
        
        val nextPageList = tryGet(a, "class").map( {
            case s if (s == "next page-numbers") => getUALT18(tryGet(a,"href").get)
            case _ => el } ).getOrElse(el)
            
        res ++= nextPageList
      } 
    }     
  }
}