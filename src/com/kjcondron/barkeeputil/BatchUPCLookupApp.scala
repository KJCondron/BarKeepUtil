package com.kjcondron.barkeeputil

import com.kjcondron.scalaqlite._


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
