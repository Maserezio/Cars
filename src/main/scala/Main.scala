
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._


import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object Main {
  //case class Car( id:Int, name:String, founded:Int, founder:List[String], country:String, website:String)

  def main(args: Array[String]) = {
    val browser = JsoupBrowser()
    val page = browser.get("https://www.carlogos.org/car-brands/honda-logo.html")

    val info = ListBuffer.empty ++= (page >> elementList(".brand-overview p").map(_ >> allText("p")))
      .filter(el => el.contains("Founded") || el.contains("Founder") || el.contains("Headquarters") || el.contains("Official Site"))

    val fin = info.toList
    for (x <- info) {
      println(x.substring(x.indexOf(':') + 2))
    }

    println("")

    val str = "Minato, Tokyo, Japan"
    val reg: Regex = "(, )([\\s\\w]+)$".r

    

    for (patternMatch <- reg.findAllMatchIn(str))
      println(patternMatch.group(2))
  }
}

