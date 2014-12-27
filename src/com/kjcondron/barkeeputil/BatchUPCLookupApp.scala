package com.kjcondron.barkeeputil

import com.kjcondron.scalaqlite._

object UAlt18 extends App {
 val address = """http://theunofficialalt18countdownplaylists.com/"""
   val res = UPCLookup.getUALT18Cal(address)
   res.foreach(println)
   UPCLookup.shutdown
}


object Alt18 extends App {
  
  def splitStringOnce( sep : Char )( str : String ) =
  {
    str.split(sep) match {
      case Array(t1,t2) => (t1.trim,t2.trim)
      case _ => ( str.takeWhile(_!=sep).trim, str.dropWhile(_!=sep).drop(1).trim ) 
    }
  }
  
  def stringGrouper( str : String ) : String = 
    str.toUpperCase().filter(c => c.toInt > 64 && c.toInt < 91)
   
 //val test = """http://blog.siriusxm.com/2013/08/31/alt-nation-alt-18-countdown-83113/"""
// UPCLookup.getALT181W(test)
    
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

object BatchUPCLookup extends App {
  
  val upcRows = BatchUPCHelper.getInventory("Apt")
  val header = BatchUPCHelper.getInvHeader
  header.foreach(println)
  val idx = header.indexOf("upc")
  println(idx)
  
  upcRows.foreach( r => {
    println( r.mkString("PRODROW:", ",", "") + ":" + UPCLookup.getResult( r(idx).toString ).mkString("HTTPRES:", ",", "" ))
    Thread.sleep(10000)
  })
  
  UPCLookup.shutdown  
}

import com.kjcondron.scalaqlite.ResultSetIterator._

object BatchUPCHelper
{
  final val db = """C:\Users\Karl\Documents\GitHub\BarKeepUtil\Data\db\barkeep__20140216"""
  val (_,_,conn) = SQLiteHelper.getDBDetails(db)
 // final val BAR_SQL = """select * from vInventory where bar_id=1"""
 
  def getBarId( barName : String ) = {
	 val res = SQLiteHelper.getTableRS("Bars", conn)
	 val mp = res.map( r => {
	   
	   val value = r(0) match {
	     case IntResult(i) => i
	     case _ => throw new Exception("Expected Integer found " + r(0).getType)
	   }
	   val key = r(1) match {
	     case StringResult(s) => s
	     case _ => throw new Exception("Expected String found " + r(1).getType)
	   }
	   key-> value
	 }).toMap
	 
	 mp(barName)
  }
	
  def getInventory( barName : String ) = {
	
	 val barId = getBarId(barName)
	 
	 val invRes = SQLiteHelper.getTableRS("vInventory", conn)
	 invRes.filter( r => r(1) == IntResult(barId)  )
 }
  
  def getInvHeader() = {
   val invRes = SQLiteHelper.getTableRS("vInventory", conn)
   SQLiteHelper.getColNames(invRes)
  }
  
}
