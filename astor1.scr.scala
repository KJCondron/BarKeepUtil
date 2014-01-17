val getPg = (s:String) => { println(s); io.Source.fromURL(s)(scala.io.Codec.ISO8859).getLines }

def takeBetween[T]( xs : Iterator[T], start : T=>Boolean, stop : T=>Boolean ) : Iterator[T] =
	xs.dropWhile(!start(_)).drop(1).takeWhile(!stop(_))

val start = (x:String) => x.contains( """Choose a Spirit Type""" )
val stop  = (x:String) => x.contains( """/select""" )
	
val topLevelLoc = """http://www.astorwines.com/SpiritsSearch.aspx"""
val topLevelSource = getPg(topLevelLoc)

val spirits = takeBetween( topLevelSource, start, stop ).map(_.trim).filter( _.size != 0 ).toList
val styles = spirits.map( _.drop(15).take(4)  ).sorted

val styleURL = """http://www.astorwines.com/SpiritsSearchResult.aspx?p=2&search=Advanced&searchtype=Contains&term=&cat=2&style="""

val results = styles.map( x => getPg(styleURL ++ x).filter( _.contains("""Results for""") ).next )
val pages = styles.map( x => getPg(styleURL ++ x).filter( _.contains("""Go to Page""") ).next )

/*
val maxPages = pages.map( x => {
		val srchStr = "Page="
		val idx = x.lastIndexOf(srchStr)
		if(idx == -1 )
			"1"
		else
		{
			val stIdx = idx+srchStr.size
			val edIdx = x.drop(stIdx).indexOf("'")
			x.drop(stIdx).take(edIdx)
		}
	})
	
*/