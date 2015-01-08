package com.kjcondron.music

import com.kjcondron.barkeeputil.UPCLookup
import com.kjcondron.web.HTTPWrapper

object Alt18 extends App {
  
  def splitStringOnce( sep : Char )( str : String ) =
  {
    str.split(sep) match {
      case Array(t1,t2) => (t1.trim,t2.trim)
      case _ => ( str.takeWhile(_!=sep).trim, str.dropWhile(_!=sep).drop(1).trim ) 
    }
  }
  
  def stringGrouper( str : String ) : String = 
    str.toUpperCase().filter(c => c.toInt > 64 && c.toInt < 91).replace("THE","").replace("AND","")
    
  val test = """http://blog.siriusxm.com/tag/alt-18/alt-nation-alt-18-countdown-1613"""
  val conn = new HTTPWrapper("""C:\Users\Karl\Documents\ALT18\""")
  val HTML = conn.request(test)
      
  val address = """http://blog.siriusxm.com/tag/alt-18/"""
  val allPages = List(address) ++ (2 to 6).map(address + """page/"""+ _ + """/""")
  val res = allPages.flatMap(UPCLookup.getALT18)
  val AAndS = res.map(splitStringOnce('–'))
  val allItems = AAndS.distinct
  
  val groupedItems = allItems.groupBy{ case (name,_) => stringGrouper(name) }
  val filterdItems = groupedItems.map( x =>{ 
    val bandName = x._2.head._1
    val songs = x._2.map(_._2)
    val grpSngs = songs.groupBy(stringGrouper)
    val distinctSongs = grpSngs.map(_._2.head)
    (bandName, distinctSongs)
  } )
  
  allItems.foreach(println)
  
  val bands = allItems.map(_._1).distinct // throw out perfect matches first?
  val groupdBands = bands.groupBy( name => name.toUpperCase().filter(c => c.toInt > 64 && c.toInt < 91) )
  val allBands = groupdBands.map(_._2.head)
  
  allBands.foreach(println)
   
 /* val flattend = allBands.map( band => {
    val songs = allItems.filter(_._1==band).map(_._2)
    (band, songs)
  })
  
  flattend.foreach( e => {
    print(e._1 + ":")
    println(e._2.reduceLeft( (acc,e) => acc + " :: " + e ))
  }) */
  
  filterdItems.foreach( e => {
    print(e._1 + ":")
    println(e._2.reduceLeft( (acc,e) => acc + " :: " + e ))
  })
  
  UPCLookup.shutdown  
}

