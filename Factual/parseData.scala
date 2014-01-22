import util.parsing.json._
val lf = (loc:String) => io.Source.fromFile(loc).getLines
val data = lf("""C:\Users\Karl\Documents\GitHub\BarKeepUtil\data.txt""")
val data2 = data.map(_.drop(46))
val data3 = data2.map(_.split(""","included_rows"""")(0))
val data4 = data3.flatMap(JSON.parseFull(_)) // flatMap throws away Nones
val data5 = data4.map( _.asInstanceOf[List[Map[String,Any]]] ).toList

val keys = data5.flatMap(_.map(_.keys))
val allFields = keys.flatten.distinct
val commonFields = allFields.filter( x=>
    keys.foldLeft(true)( (acc,e) =>acc && e.exists(_==x) ))

val bs=List("Whisky", "Gin", "Vodka", "Tequila", "Rum", "Brandy", "Bourbon", "Whiskey", "Liqueur", "Dekuyper")
 
val data6 = data5.flatten
val upcs = data6.filter( _.keys.toSet.contains("upc") )
val upcFields = upcs.flatMap(_.keys.toSet)
val upcAll = upcFields.distinct

val upcCommon = upcAll.filter( x=>
    upcs.foldLeft(true)( (acc,e) =>acc && e.keys.toSet.contains(x) ))

val cats = bs.map( x => {
    x -> upcs.filter( u => u.values.exists( f => f.toString.contains(x) ) ) } )

import java.io.PrintWriter
cats.foreach ( { case(cat, productList) =>
    val p = new PrintWriter("""C:\Users\Karl\Documents\GitHub\BarKeepUtil\""" + cat + """.txt""" )
    productList.foreach( product => { 
		upcCommon.foreach( field => product.get(field).foreach( x=>p.print(x+";") ))
		p.print( product.getOrElse("brand", p.print("no brand")) ) 
		p.println })
	p.close
	} )
    











