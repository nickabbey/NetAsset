import scala.actors._
import Actor._
import scala.collection.mutable.HashMap

object FindTotalWorthConcurrent {

  def main(args: Array[String]): Unit = {

    var isSuspect = false
    val symbolsAndNames = getNamesAndSymbols()
    val symbolsAndUnits = StockPriceFinder.getTickersAndUnits

    val caller = self

    println("Today is " + new java.util.Date())
    println("Ticker   Company Name                           Units   Closing Price($) Total Value($)")

    val startTime = System.nanoTime()

    symbolsAndUnits.keys.foreach { symbol =>
      actor { caller ! (symbol, StockPriceFinder.getLatestClosingPrice(symbol)) }
    }

    //println(symbolsAndUnits.size)
    val netWorth = (1 to symbolsAndUnits.size).foldLeft(List[Tuple5[String, String, Int, Double, Double]]()) { (worth, index) =>
      receiveWithin(10000) {
        case (symbol: String, latestClosingPrice: Double) =>
          val name = symbolsAndNames(symbol)
          val units = symbolsAndUnits(symbol)
          val value = units * latestClosingPrice
          val tuple5 = (symbol, name, units, latestClosingPrice, value)
          tuple5 :: worth

        case TIMEOUT =>
          isSuspect = true
          worth
      }
    }

    if (isSorted) {
      val sortedList = netWorth.sortBy(_._1)
      printFormattedOutput(sortedList, isSorted, isSuspect, (System.nanoTime() - startTime))
    } else printFormattedOutput(netWorth, isSorted, isSuspect, (System.nanoTime() - startTime))

  }

  //reused Ronz code from StockPriceFinder.scala, modified to generate a map of K:V Symbol:Name
  def getNamesAndSymbols() = {
    val stocksAndUnitsXML = scala.xml.XML.load("stocks.xml")
    (stocksAndUnitsXML \ "symbol").foldLeft(HashMap[String, String]()) { (map, symbolNode) =>
      val ticker = (symbolNode \ "@ticker").toString
      val name = (symbolNode \ "name").text.toString
      map(ticker) = name //Creates and returns a new Map
      map
    }
  }

  //check the stocks.xml file, return true if it contains the sorted = true tag, false otherwise
  def isSorted(): Boolean = {
    val stocksAndUnitsXML = scala.xml.XML.load("stocks.xml")
    val sorted = (stocksAndUnitsXML \ "@sort").toString().trim()
    //println("function isSorted found " + sorted + " in the stocks.xml file")
    if (sorted.equalsIgnoreCase("true")) true else false
  }

  //Do the output
  def printFormattedOutput(compositeList: List[Tuple5[String, String, Int, Double, Double]], isSorted: Boolean, isSuspect: Boolean, time: Long) {
    //sort by ticker symbol if the stocks.xml file says so

    //otherwise, just print it in the order it's already in.
    var totalWorth = 0.0
    compositeList.foreach(symbol => {
      totalWorth += symbol._5.toDouble
      println("%-7s %-40s %-3d      %7.3f       %10.3f".format(symbol._1, symbol._2, symbol._3, symbol._4, symbol._5))
    })

    //print the summary line including a ? if any results didn't come back
    if (isSuspect) println("The total value of your investments is $" + totalWorth) else println("The total value of your investments is $" + totalWorth + "?")
    println("Took %f seconds".format((time) / 1000000000.0))
  }

}