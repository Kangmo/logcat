/*
   How to run :
   1. Download archived log files for each day.
   2. move archived log files into ../thelog folder.
   3. execute gunzip * in the thelog folder.
   3. execute sbt command.
   4. type ~run
*/


import scala.io.Source

case class Token(val value : String, var count : Int = 0, var isFeature : Boolean = false)
class TokensCategory(val tokens: Array[Token]) {
}

object LogParser {
	def parse(logLine:String) = {
		val tokens = logLine.split("\t")
		( tokens.take( tokens.size - 1), tokens.last )
	}
	def getLogTokens(log:String) = {
		val tokenizer = new java.util.StringTokenizer(log, ":\", =()[]{}<>';")
		val tokens = new scala.collection.mutable.ArrayBuffer[String]()
		while(tokenizer.hasMoreTokens() ) {
			tokens.append( tokenizer.nextToken() )
		}
		tokens.map( Token(_) ).toArray
	}
	def extractFeatureTokens(tokens:Array[String]) = tokens
}

object LogCategorizer {
	def main(args: Array[String]) {
		val files = new java.io.File("../thelog").listFiles()

		val tokenCountMap = scala.collection.mutable.Map[Int, scala.collection.mutable.ArrayBuffer[TokensCategory]]()

		files.map { file =>
			var line = ""
		  	for(line <- Source.fromFile(file).getLines()) {
		  		val (headers:Array[String], log:String) = LogParser.parse(line)
/*
		  		println("======================")
		  		println(log)
		  		println("----------------------")
		  		headers.map{ t =>
			  		println(t)
		  		}
*/
		  		val logTokens = LogParser.getLogTokens(log)
		  		
		  		val tokenCat = tokenCountMap.get(logTokens.length);
		  		val cat = if (tokenCat.isDefined)
		  		{
		  			tokenCat.get
		  		}
		  		else 
		  		{
		  			val category = new scala.collection.mutable.ArrayBuffer[TokensCategory]();
		  			tokenCountMap(logTokens.length) = category;
		  			category
		  		}

		  		cat.append( new TokensCategory(logTokens) )
/*
		  		if ( logTokens.length > 100)
		  		{
			  		println("----------------------")
		  			println(log)
			  		println("----------------------")
			  		logTokens.map{ t =>
				  		println(t)
			  		}
		  		}
*/
		  		//learner.add(headers, logTokens, log)
		  	}
		}

		// count tokens at the same position
		tokenCountMap.map {
			case (tokenCount, catArray) => {
//				println("token length = " + tokenCount)
//				println("category count = " + catArray.length)

				// For each token at poistion, set token count grouped by token 
				for (i <- 0 until tokenCount) {
//					println("token indx = " + i)

					val sortedTokens = catArray.map{
						_.tokens(i)
					}.sortBy(_.value)

					val groups = sortedTokens.groupBy(_.value)
					groups.map {
						case (group, tokens) =>
							tokens.foreach {
								t => t.count = tokens.length
							}
					}
				}

				// For each category, get the token with maximum number of occurence.
				// Set all tokens having the same occurrence as a "feature" token.
				for (cat <- catArray) {
					val tokenWithMaxCount = cat.tokens.max(Ordering.by((_:Token).count))
					cat.tokens.foreach { t =>
						if (t.count == tokenWithMaxCount.count)
							t.isFeature = true;
					}
				}
			}
		}

		// Treat a token a noise token if it is not a feature token.
		// Group categories by feature tokens
		tokenCountMap.map {
			case (tokenCount, catArray) => {
				println("=================================================================================")
				println(s"Log Category with token length = ${tokenCount}. Total count = ${catArray.length}"  )
				println("=================================================================================")

				val groups = catArray.map { cat =>
					val log = cat.tokens.map { t =>
						if ( t.isFeature ) t.value else "_"
					}.mkString(",")
					log
				}.groupBy(_.toString)

				groups.map {
					case (group, groupArray) =>
						println( s"[${groupArray.length}]${group}")
				}	
			} 

		}
	}
}
