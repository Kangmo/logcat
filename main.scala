import scala.io.Source

case class Token(val value : String, var count : Int = 0, var isFeature : Boolean = false)
class LogEntry(val line : String, val tokens: Array[Token]) {
	def featureSummary = {
		tokens.map { t =>
			if ( t.isFeature ) t.value else "_"
		}.mkString(",")
	}
}

object Helper {
	def isIpAddress(string : String) =
		"""(\d)+.(\d)+.(\d)+.(\d)""".r.findFirstMatchIn(string).isDefined
	def isNoise(string : String) = isIpAddress(string)
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

		val tokenCountMap = scala.collection.mutable.Map[Int, scala.collection.mutable.ArrayBuffer[LogEntry]]()
		
		files.map { file =>
			println("****************************************")
			println(file.toString)
			println("****************************************")

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
		  			val category = new scala.collection.mutable.ArrayBuffer[LogEntry]();
		  			tokenCountMap(logTokens.length) = category;
		  			category
		  		}

		  		cat.append( new LogEntry(line, logTokens) )
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
					// Sort the tokens at position i.
					val sortedTokens = catArray.map{
						_.tokens(i)
					}.sortBy(_.value)

					// Group by the token value to calculate the occurrence of the token.
					// The occurence of the token is set to Token.count
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
					// Get rid of noise such as IP address
					val featureCandidates = cat.tokens.
							filter{t=> !Helper.isNoise(t.value)}
					
					if ( ! featureCandidates.isEmpty ) {
						// Get the token which has maximum occurence at the position.
						val tokenWithMaxCount = 
							featureCandidates.max(Ordering.by((_:Token).count))

						// Mark a token as a feature token, if the number of occurence equals to a token which has the maximum occurence.
						featureCandidates.foreach { t =>
							if (t.count == tokenWithMaxCount.count)
								t.isFeature = true;
						}
					}
				}
			}
		}

		// Treat a token a noise token if it is not a feature token.
		// Group categories by feature tokens
		tokenCountMap.map {
			case (tokenCount, catArray) => {
				println("")
				println("=================================================================================")
				println(s"Log Category with token length = ${tokenCount}. Total count = ${catArray.length}"  )
				println("=================================================================================")
				println("")

				val groups = catArray.groupBy(_.featureSummary)

				groups.map {
					case (group, groupArray) =>
						val exampleLogEntry = groupArray(0)
						println( s"[${groupArray.length}] ${group}")
						println( s"${exampleLogEntry.line}")
						println( "")
				}	
			} 

		}		  	
	}
}
