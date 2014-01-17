 val baseURL="""http://api.v3.factual.com/t/products-cpg?filters={%22$and%22:[{%22category%22:{%22$eq%22:%22ALCOHOLIC+BEVERAGES%22}}]}&KEY=qr2kn2NxC693ox3IGice7gmarbt7og1oKS2xFi64&q="""
 val bs=List("Whisky", "Gin", "Vodka", "Tequila", "Rum", "Brandy", "Bourbon", "Whiskey", "Liqueur", "Dekuyper")
 val dataIter = bs.map( x=>io.Source.fromURL( baseURL + x +"&include_count=true" ).getLines)
 val data=dataIter.map(_.toList).flatten
 val counts = data.map( x=>x.drop(x.indexOf("total_row_count")+17).takeWhile(_!='}').toInt )
 val bsMap = (bs zip counts).toMap
 
 val rawData = bsMap.map( { case(k,v) => 
    val rs = Range(0,math.min(v,81),20)
    rs.map( x => {
    val u = baseURL + k + "&offset=" + x
    println(u)
    io.Source.fromURL( u ).getLines 
    }) } )
    
 