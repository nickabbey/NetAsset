/**
 * Programming Scala: Tackle Multi-Core Complexity on the Java Virtual Machine
 * by Venkat Subramaniam
 * 
 * Modified by Nick Abbruzzese for Ron Coleman's Network Programming in Scala Course
 * Marist College, Spring 2013
 */
import scala.collection.mutable.HashMap
import scala.io.Source

object Main {
  def main(args: Array[String]) {
    println("Today is " + new java.util.Date())
    println("Ticker  Units  Closing Price($) Total Value($)")
    
    val symbolsAndUnits = StockPriceFinder.getTickersAndUnits
    
//    val symbols = symbolsAndUnits.keysIterator.toList

    val startTime = System.nanoTime()
    
//    val symbolsQuotes = StockPriceFinderBetter.getLatestClosingPrices(symbols)

    val netWorth = symbolsAndUnits.foldLeft(0.0) { (worth, tickerAndShares) =>
      val (ticker, shares) = tickerAndShares

      val latestClosingPrice = StockPriceFinder.getLatestClosingPrice(ticker)
      
//      val latestClosingPrice = symbolsQuotes(ticker)

      val value = shares * latestClosingPrice

      println("%-7s  %-5d  %-16f  %f".format(ticker, shares, latestClosingPrice, value))

      worth + value
    }
    val endTime = System.nanoTime()

    println("The total value of your investments is $" + netWorth)
    println("Took %f seconds".format((endTime - startTime) / 1000000000.0))
  }
}

object StockPriceFinder {
  def getLatestClosingPrice(symbol: String) = {
    val url = "http://ichart.finance.yahoo.com/table.csv?s=" +
      symbol + "&a=00&b=01&c=" + new java.util.Date().getYear

    val data = scala.io.Source.fromURL(url).mkString
    val mostRecentData = data.split("\n")(1)
    val closingPrice = mostRecentData.split(",")(4).toDouble
    closingPrice
  }

  def getTickersAndUnits() = {
    val stocksAndUnitsXML = scala.xml.XML.load("stocks.xml")

    (stocksAndUnitsXML \ "symbol").foldLeft(HashMap[String, Int]()) { (map, symbolNode) =>
      val ticker = (symbolNode \ "@ticker").toString
      val units = (symbolNode \ "units").text.toInt
      map(ticker) = units //Creates and returns a new Map
      map
    }
  }
}

/**
 * Improves the original stock price finder 
 * @author Ron Coleman, Ph.D.
 */
object StockPriceFinderBetter {
  val REST_URL = "http://finance.yahoo.com/d/quotes.csv?s="
    
  /**
   * Gets the stock quotes in one fell swoop given a list.
   * @param tickers List of ticker symbols
   */
  def getLatestClosingPrices(tickers : List[String]) : HashMap[String,Double] = {
    // Validate the list of tickers
    if(tickers == null || tickers.size == 0)
      return HashMap[String,Double]()
      
    // Build the "gummy" sym0+sym1+...symn portion of the url
    val syms = tickers.foldLeft("") { (str, ticker) => 
      if(str.size == 0)
        str + ticker 
      else
        str + "+" + ticker
    }
    
    // Complete the url
    val url = REST_URL + syms + "&f=sl1"
    
    // Get the quotes from Yahoo
    val records = Source.fromURL(url).mkString.split("\n")
    
    // Convert the quotes to a hash map
    val symsQuotes =
      records.foldLeft(HashMap[String, Double]()) { (map, record) =>
        // Split the record into sym and quote collection
        val symQuote = record.split(",")

        // Since the sym comes in as "sym" we need to remove the ""
        val sym = symQuote(0).split("\"")(1)

        // Convert the quote to a double precision
        val quote = symQuote(1).toDouble

        // Store the quote in the map at location sym
        map(sym) = quote

        // Needed by foldLeft since we're folding into a HashMap
        map
      }
    
    // Return the sym-quote hash
    symsQuotes
  }
}
